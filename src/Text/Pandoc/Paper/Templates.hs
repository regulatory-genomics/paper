{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Text.Pandoc.Paper.Templates
    ( loadTemplate
    , defaultLaTeXTemplate
    , defaultHtmlTemplate
    ) where

import Text.Pandoc
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO, MonadIO)

import Text.Pandoc.Paper.Internal (latexTemplateFile, htmlTemplateFile)

loadTemplate :: FilePath -> PandocIO (Template T.Text)
loadTemplate fl = do
    template <- liftIO $ T.readFile fl
    runWithPartials (compileTemplate "" template) >>= \case
        Left err -> error err
        Right t -> return t

defaultLaTeXTemplate :: PandocIO (Template T.Text)
defaultLaTeXTemplate =
    runWithPartials (compileTemplate "" $ T.pack $ B.unpack latexTemplateFile) >>= \case
        Left err -> error err
        Right x -> return x

defaultHtmlTemplate :: PandocIO (Template T.Text)
defaultHtmlTemplate =
    runWithPartials (compileTemplate "" $ T.pack $ B.unpack htmlTemplateFile) >>= \case
        Left err -> error err
        Right x -> return x