{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (when)
import Text.Pandoc
import Text.DocTemplates (toContext)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Shelly (shelly, bash_, cp, liftIO, mkdir_p)
import System.FilePath.Posix (takeBaseName, takeDirectory, takeFileName)
import System.IO.Temp (withTempFile)
import System.IO (hClose)
import Data.Default
import           Data.Version                      (showVersion)
import           Options.Applicative
import           Paths_paper(version)
import           Text.Printf

import Text.Pandoc.Paper.Writers (writeDocx')
import Text.Pandoc.Paper.Readers (readYaml)
import Text.Pandoc.Paper.Filters
import Text.Pandoc.Paper.Templates
import Text.Pandoc.Paper.CSL

data Options = Options
    { _input :: FilePath
    , _latex_template :: Maybe FilePath
    , _html_template :: Maybe FilePath
    , _docx_template :: Maybe FilePath
    , _bib_cache :: FilePath
    , _out_dir :: Maybe FilePath
    , _output_pdf :: Bool
    , _output_docx :: Bool
    , _output_html :: Bool
    , _disable_cache :: Bool
    } deriving (Show, Read)

-- The parser for the command line options
optsParser:: Parser Options
optsParser = Options
    <$> strArgument (metavar "INPUT")
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

defaultMain :: Options -> IO ()
defaultMain Options{..} = runIOorExplode $ do
    setVerbosity INFO
    shelly $ mkdir_p outputDir
    doc <- (crossref <$> readYaml _input) >>=
        citeproc (if _disable_cache then Nothing else Just _bib_cache) (Just cslNature) >>=
        absPath
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
        let opts = def
                { writerTemplate=Just htmlTemplate
                , writerVariables = toContext $ M.fromList [("self-contained" :: T.Text, "true" :: T.Text)] }
        writeHtml5String opts (addAuthors "html" doc) >>=
            liftIO . T.writeFile (filepath <> ".html")
    when _output_docx $
        writeDocx' def{ writerReferenceDoc=_docx_template} (addAuthors "docx" doc) >>=
            liftIO . BL.writeFile (filepath <> ".docx")
  where
    filepath = outputDir <> "/" <> takeBaseName _input
    outputDir = case _out_dir of
        Nothing -> takeDirectory _input
        Just dir -> dir

main :: IO ()
main = execParser opts >>= defaultMain
  where
    opts = info (helper <*> optsParser) ( fullDesc <>
        header (printf "paper-v%s" (showVersion version)) )