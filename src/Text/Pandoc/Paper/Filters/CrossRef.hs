{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Paper.Filters.CrossRef
    ( crossref
    ) where

import           Control.Monad
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import           Data.List
import           System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.CrossRef
import Text.Pandoc.Builder

-- | This function traverses the AST and add cross references for figures and tables
crossref :: Pandoc -> Pandoc
crossref p@(Pandoc meta blks) = case nSupp of
    0 -> Pandoc meta' blk'
    _ -> let revBlk = reverse blk'
             supp = reverse $ take nSupp revBlk
             rest = reverse $ drop nSupp revBlk
          in Pandoc (setMeta "supplement" (MetaBlocks supp) meta') rest
  where
    Pandoc meta' blk' = runCrossRef template Nothing crossRefAction doc
    (nSupp, doc) = case lookupMeta "supplement" meta of
        Just (MetaBlocks supplement) -> (length supplement, Pandoc meta $ blks <> supplement)
        Nothing -> (0, Pandoc meta blks)
    template = figureTitle ("Figure" :: String) <>
        titleDelim ("|" :: String) <>
        tp <> meta
    tp = figureTemplate $ strong
        (displayMath "figureTitle" <> space <> displayMath "i" <> space <> displayMath "titleDelim")
        <> space <> displayMath "t"

crossRefAction :: Pandoc -> CrossRefM Pandoc
crossRefAction (Pandoc meta bs) = do
    bs' <- crossRefBlocks bs
    return $ Pandoc meta bs'