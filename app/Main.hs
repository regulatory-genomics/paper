{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (when, forM_)
import Text.Pandoc
import Text.DocTemplates (toContext)
import Text.Pandoc.SelfContained (makeSelfContained)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Shelly (shelly, bash_, cp, liftIO, mkdir_p, test_d)
import System.FilePath.Posix (takeBaseName, takeDirectory)
import System.Directory (setCurrentDirectory, makeAbsolute)
import           Data.Version                      (showVersion)
import           Options.Applicative
import           Paths_paper(version)
import           Text.Printf
import Text.Pandoc.Shared (stringify)
import Data.Aeson.Encode.Pretty (encodePretty)

import Text.Pandoc.Paper.Writers (writeDocx', writeHtml)
import Text.Pandoc.Paper.Readers (readYaml)
import Text.Pandoc.Paper.Filters
import Text.Pandoc.Paper.Templates
import Text.Pandoc.Paper.CSL
import Text.Pandoc.Paper.Internal (template)

data Command = Init InitOpts | Build BuildOpts
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
    )

initParser:: Parser InitOpts
initParser = InitOpts
    <$> strArgument (metavar "PROJECT_DIR")
 
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
    forM_ template $ \(file, content) -> B.writeFile (_proj_dir <> "/" <> file) content

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
    let filepath = case lookupMeta "short-title" meta of
            Just title -> outDir <> "/" <> T.unpack (stringify title)
            _ -> outDir <> "/paper"
    
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

main :: IO ()
main = execParser opts >>= defaultMain
  where
    opts = info (helper <*> optsParser) ( fullDesc <>
        header (printf "paper-v%s" (showVersion version)) )