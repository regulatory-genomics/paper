{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Paper.Writers
    ( writeDocx'
    , writeHtml
    ) where

import Text.Pandoc
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad.IO.Class
import System.IO.Temp (withSystemTempFile)
import System.IO

import Text.Pandoc.Paper.Internal (docxTemplateFile)

writeDocx' :: WriterOptions -> Pandoc -> PandocIO BL.ByteString
writeDocx' opts (Pandoc meta doc) = withSystemTempFile "tmp.dotx" $ \fl h -> do
    opts' <- case writerReferenceDoc opts of
        Just _ -> return opts
        Nothing -> do
            liftIO $ B.hPutStr h docxTemplateFile >> hClose h
            return $ opts{writerReferenceDoc=Just fl}
    writeDocx opts' $ Pandoc meta $ doc ++ refs ++ supplement ++ supp_refs
  where
    refs = case lookupMeta "refs" meta of
        Just (MetaBlocks blk) -> blk
        _ -> []
    supplement = case lookupMeta "supplement" meta of
        Just (MetaBlocks blk) -> blk
        _ -> []
    supp_refs = case lookupMeta "supp_refs" meta of
        Just (MetaBlocks blk) -> blk
        _ -> []

writeHtml :: WriterOptions -> Pandoc -> PandocIO T.Text
writeHtml opts (Pandoc meta doc) = writeHtml5String opts' document
  where
    opts' = opts{writerMathMethod = MathJax "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"}
    document = Pandoc meta $ doc ++ refs ++ supplement ++ supp_refs
    refs = case lookupMeta "refs" meta of
        Just (MetaBlocks blk) -> blk
        _ -> []
    supplement = case lookupMeta "supplement" meta of
        Just (MetaBlocks blk) -> blk
        _ -> []
    supp_refs = case lookupMeta "supp_refs" meta of
        Just (MetaBlocks blk) -> blk
        _ -> []
