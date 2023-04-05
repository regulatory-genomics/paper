{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Paper.Readers (readYaml, readDoc) where

import Control.Monad
import Data.Yaml (Value(..), toJSON)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Text.Pandoc
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Shared (stringify)
import Data.Default
import Text.Pandoc.PDF
import Text.Pandoc.Writers.ConTeXt
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import qualified Data.Map as M
import Text.Pandoc.Walk
import System.FilePath (takeDirectory)

-- | Read a document from a Yaml file.
readYaml :: FilePath -> PandocIO Pandoc
readYaml file = do
    let dir = takeDirectory file
    metadata <- liftIO (B.readFile file) >>= yamlToMeta def{ readerExtensions = pandocExtensions } Nothing
    Pandoc meta doc <- case lookupMeta "contents" metadata of
        Just (MetaList contents) -> fmap mconcat $ forM contents $ \filename ->
            readDoc $ dir <> "/" <> T.unpack (stringify filename)
        Nothing -> error "No 'contents' key found in YAML file."
        _ -> error "The 'contents' key must be a list of strings."
    return $ Pandoc (meta <> metadata) doc

readDoc :: FilePath -> PandocIO Pandoc
readDoc txtMain = do
    liftIO (T.readFile txtMain) >>= readMarkdown def{ readerExtensions = pandocExtensions }