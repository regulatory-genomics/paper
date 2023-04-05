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
crossref p@(Pandoc meta _) =
    runCrossRef meta' Nothing crossRefAction p
  where
    meta' = figureTitle ("Figure" :: String) <>
        titleDelim ("|" :: String) <>
        tp <> meta
    tp = figureTemplate $ strong
        (displayMath "figureTitle" <> space <> displayMath "i" <> space <> displayMath "titleDelim")
        <> space <> displayMath "t"

crossRefAction :: Pandoc -> CrossRefM Pandoc
crossRefAction (Pandoc meta bs) = do
    bs' <- crossRefBlocks bs
    return $ Pandoc meta bs'