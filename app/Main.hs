{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad (when, forM_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import System.Exit (ExitCode (ExitFailure), exitWith)
import Data.Char (isControl)
import Text.Pandoc
import Text.Pandoc.SelfContained (makeSelfContained)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Shelly (shelly, bash_, liftIO, mkdir_p, test_d)
import System.FilePath.Posix (takeDirectory, (</>))
import System.Directory (setCurrentDirectory, makeAbsolute)
import           Data.Version                      (showVersion)
import           Options.Applicative
import           Paths_paper(version)
import           Text.Printf
import Text.Pandoc.Shared (stringify)
import Data.Aeson.Encode.Pretty (encodePretty)

import Text.Pandoc.Paper.Writers (writeDocx', writeHtml)
import Text.Pandoc.Paper.Readers (readYaml, readYamlWithSources)
import Text.Pandoc.Paper.Editor (inspectValue, planEditValue)
import Text.Pandoc.Paper.Filters
import Text.Pandoc.Paper.Templates
import Text.Pandoc.Paper.CSL
import Text.Pandoc.Paper.Internal (template)

data Command = Init InitOpts | Build BuildOpts | Describe FilePath | Inspect FilePath | PlanEdit FilePath
    deriving (Show, Read)

data InitOpts = InitOpts
    { _proj_dir :: FilePath
    } deriving (Show, Read)

data BuildOpts = BuildOpts
    { _input_dir :: FilePath
    , _latex_template :: Maybe FilePath
    , _html_template :: Maybe FilePath
    , _docx_template :: Maybe FilePath
    , _bib_cache :: FilePath
    , _out_dir :: Maybe FilePath
    , _output_stem :: Maybe FilePath
    , _output_raw :: Bool
    , _output_pdf :: Bool
    , _output_docx :: Bool
    , _output_html :: Bool
    , _disable_cache :: Bool
    , _self_contained :: Bool
    , _no_embed_fig :: Bool
    } deriving (Show, Read)

optsParser :: Parser Command
optsParser = hsubparser
    ( command "init" (info (Init <$> initParser) (progDesc "Initialize a new project"))
   <> command "build" (info (Build <$> buildParser) (progDesc "Build a project"))
   <> command "describe" (info describeParser (progDesc "Describe a project without building or changing it"))
   <> command "inspect" (info inspectParser (progDesc "Report source-mapped editable blocks without changing the project"))
   <> command "plan-edit" (info planEditParser (progDesc "Validate one block replacement from stdin without writing it"))
    )

initParser:: Parser InitOpts
initParser = InitOpts
    <$> strArgument (metavar "PROJECT_DIR")

describeParser :: Parser Command
describeParser = Describe <$> strArgument (metavar "PROJECT_DIR")
    <* flag' () (long "json" <> help "Emit versioned JSON to stdout")

inspectParser :: Parser Command
inspectParser = Inspect <$> strArgument (metavar "PROJECT_DIR")
    <* optional (flag' () (long "json" <> help "Emit versioned JSON to stdout"))

planEditParser :: Parser Command
planEditParser = PlanEdit <$> strArgument (metavar "PROJECT_DIR")
    <* optional (flag' () (long "json" <> help "Emit versioned JSON to stdout"))
 
buildParser:: Parser BuildOpts
buildParser = BuildOpts
    <$> strArgument (metavar "PROJECT_DIR")
    <*> (optional . strOption)
        ( long "latex-template"
       <> metavar "LATEX_TEMPLATE"
       <> help "LaTeX template file." )
    <*> (optional . strOption)
        ( long "html-template"
       <> metavar "HTML_TEMPLATE"
       <> help "HTML template file." )
    <*> (optional . strOption)
        ( long "docx-template"
       <> metavar "DOCX_TEMPLATE"
       <> help "DOCX template file." )
    <*> strOption
        ( long "bib-cache"
       <> short 'b'
       <> metavar "BIBLIOGRAPHY_CACHE"
       <> value "bibliography.bib"
       <> help "cache file for bibliography." )
    <*> (optional . strOption)
        ( long "out-dir"
       <> short 'o'
       <> metavar "OUTPUT_DIR"
       <> help "output directory" )
    <*> optional (option (eitherReader validateStem)
        (long "output-stem" <> metavar "NAME" <> help "Override the output basename (no directory separators)"))
    <*> switch
        ( long "output-raw"
        <> help "output abstract syntax tree" )
    <*> switch
        ( long "output-pdf"
        <> help "output pdf" )
    <*> switch
        ( long "output-docx"
        <> help "output docx" )
    <*> switch
        ( long "output-html"
        <> help "output html" )
    <*> switch
        ( long "disable-cache"
        <> help "disable cache" )
    <*> switch
        ( long "self-contained"
        <> help "self contained html" )
    <*> switch
        ( long "no-embed-fig"
        <> help "do not embed figures in html" )

initProject :: InitOpts -> IO ()
initProject InitOpts{..} = do
    shelly $ do
        exists <- test_d _proj_dir
        when exists $ error "Project directory already exists."
        mkdir_p _proj_dir
    forM_ template $ \(file, content) -> do
        shelly $ mkdir_p $ _proj_dir <> "/" <> takeDirectory file
        B.writeFile (_proj_dir <> "/" <> file) content

validateStem :: String -> Either String FilePath
validateStem name
    | null name || all (== ' ') name || name == "." || name == ".." || any invalid name
        = Left "output stem must be a single nonempty filename stem"
    | otherwise = Right name
  where
    invalid ch = isControl ch || ch `elem` ("/\\:" :: String)

defaultOutputStem :: Meta -> FilePath
defaultOutputStem meta = case lookupMeta "short-title" meta of
    Just title -> T.unpack (stringify title)
    _ -> "paper"

describeProject :: FilePath -> IO ()
describeProject project = do
    (Pandoc meta _, sources, supplements) <- runIOorExplode $ readYamlWithSources (project </> "metadata.yaml")
    BL.putStr $ encodePretty $ object
        [ "version" .= (1 :: Int)
        , "sources" .= sources
        , "supplement" .= supplements
        , "metadata" .= unMeta meta
        , "outputStem" .= defaultOutputStem meta
        ]
    BL.putStr "\n"

compileDocument :: BuildOpts -> IO ()
compileDocument BuildOpts{..} = runIOorExplode $ do
    setVerbosity INFO
    outDir <- liftIO $ makeAbsolute outputDir
    shelly $ mkdir_p outDir
    liftIO $ setCurrentDirectory _input_dir

    doc@(Pandoc meta _) <- (crossref <$> readYaml "metadata.yaml") >>=
        citeproc (if _disable_cache then Nothing else Just _bib_cache) (Just cslNature) >>=
        absPath >>=
        (if _no_embed_fig then return . placeImagesAtEnd else return)
    let filepath = outDir <> "/" <> maybe (defaultOutputStem meta) id _output_stem
    
    when _output_raw $ liftIO $ BL.writeFile (filepath <> ".raw") $ encodePretty doc

    latexTemplate <- case _latex_template of
        Just fl -> loadTemplate fl
        Nothing -> defaultLaTeXTemplate
    filterLaTeX (addAuthors "latex" doc) >>=
        writeLaTeX def{writerTemplate=Just latexTemplate} >>=
        liftIO . T.writeFile (filepath <> ".tex")

    when _output_pdf $ 
        shelly $ bash_ "tectonic" [T.pack $ filepath <> ".tex", "--chatter", "minimal"]

    when _output_html $ do
        htmlTemplate <- case _html_template of
            Just fl -> loadTemplate fl
            Nothing -> defaultHtmlTemplate
        let opts = def { writerTemplate=Just htmlTemplate }
        html <- if _self_contained
            then writeHtml opts (addAuthors "html" doc) >>= makeSelfContained
            else writeHtml opts (addAuthors "html" doc)
        liftIO $ T.writeFile (filepath <> ".html") html

    when _output_docx $
        writeDocx' def{ writerReferenceDoc=_docx_template} (addAuthors "docx" doc) >>=
            liftIO . BL.writeFile (filepath <> ".docx")
  where
    outputDir = case _out_dir of
        Nothing -> takeDirectory _input_dir
        Just dir -> dir

defaultMain :: Command -> IO ()
defaultMain (Init opts) = initProject opts
defaultMain (Build opts) = compileDocument opts
defaultMain (Describe project) = describeProject project
defaultMain (Inspect project) = inspectValue project >>= emitJson
defaultMain (PlanEdit project) = BL.getContents >>= planEditValue project >>= emitJson

-- | Print a JSON answer, and fail the process when the answer says `ok: false`
-- so a shell caller cannot mistake a rejected edit for a written one.
emitJson :: Value -> IO ()
emitJson value = do
    BL.putStr (encodePretty value)
    BL.putStr "\n"
    case value of
        Object o | KM.lookup (Key.fromText "ok") o == Just (Bool False) -> exitWith (ExitFailure 1)
        _ -> pure ()

main :: IO ()
main = execParser opts >>= defaultMain
  where
    opts = info (helper <*> optsParser) ( fullDesc <>
        header (printf "paper-v%s" (showVersion version)) )
