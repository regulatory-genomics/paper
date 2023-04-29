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
import Text.Pandoc.Builder

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
    Pandoc _ supplement <- case lookupMeta "supplement" metadata of
        Just (MetaList contents) -> fmap mconcat $ forM contents $ \filename ->
            readDoc $ dir <> "/" <> T.unpack (stringify filename)
        Nothing -> return mempty

    let meta' = setMeta "supplement" (MetaBlocks supplement) $
            setMeta "chapters" (MetaBool True) $
            setMeta "chaptersDepth" (MetaInlines [Str "1"]) $
            setMeta "chapDelim" (MetaInlines []) $
            meta <> metadata
    return $ Pandoc meta' doc

readDoc :: FilePath -> PandocIO Pandoc
readDoc txtMain = liftIO (T.readFile txtMain) >>=
    readMarkdown def{ readerExtensions = extensions }
  where
    extensions = extensionsFromList
        [ Ext_footnotes
        , Ext_inline_notes
        , Ext_pandoc_title_block
        , Ext_yaml_metadata_block
        , Ext_table_captions
        , Ext_implicit_figures
        , Ext_simple_tables
        , Ext_multiline_tables
        , Ext_grid_tables
        , Ext_pipe_tables
        , Ext_citations
        , Ext_raw_tex
        , Ext_raw_html
        , Ext_tex_math_dollars
        , Ext_latex_macros
        , Ext_fenced_code_blocks
        , Ext_fenced_code_attributes
        , Ext_backtick_code_blocks
        , Ext_inline_code_attributes
        , Ext_raw_attribute
        , Ext_markdown_in_html_blocks
        , Ext_native_divs
        , Ext_fenced_divs
        , Ext_native_spans
        , Ext_bracketed_spans
        , Ext_escaped_line_breaks
        , Ext_fancy_lists
        , Ext_startnum
        , Ext_definition_lists
        , Ext_example_lists
        , Ext_all_symbols_escapable
        , Ext_intraword_underscores
        , Ext_blank_before_blockquote
        , Ext_blank_before_header
        , Ext_space_in_atx_header
        , Ext_strikeout
        , Ext_superscript
        , Ext_subscript
        , Ext_task_lists
--        , Ext_auto_identifiers
        , Ext_header_attributes
        , Ext_link_attributes
        , Ext_implicit_header_references
        , Ext_line_blocks
        , Ext_shortcut_reference_links
        , Ext_smart
        ]