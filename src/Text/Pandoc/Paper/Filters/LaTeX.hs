{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Paper.Filters.LaTeX
    ( placeFigure
    , filterLaTeX
    ) where

import           Control.Monad
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.Walk
import           Text.Pandoc
import Text.Printf
import Text.Pandoc.Builder
import Data.Maybe

filterLaTeX :: Pandoc -> PandocIO Pandoc
filterLaTeX = walkM placeFigure

-- | This function traverses the AST and replaces all images
placeFigure :: Block -> PandocIO Block
placeFigure (Figure (ident, _, _) (Caption _ caption) fig) = do
    let classes = query getClasses fig
    caption' <- writeLaTeX def $ Pandoc (Meta M.empty) caption
    fig' <- writeLaTeX def $ Pandoc (Meta M.empty) fig
    if "side-caption" `elem` classes
        then return $ RawBlock "tex" $ T.pack $ printf
            ( "\\begin{SCfigure*}[1]\\begin{wide}"
            <> "%s"
            <> "\\caption{%s}"
            <> "\\label{%s}"
            <> "\\end{wide}\\end{SCfigure*}"
            ) (T.unpack fig') (T.unpack caption') ident
        else return $ RawBlock "tex" $ T.pack $ printf
            ( "\\begin{figure*}[t!]"
            <> "\\centering"
            <> "%s"
            <> "\\caption{%s}"
            <> "\\label{%s}"
            <> "\\end{figure*}"
            ) (T.unpack fig') (T.unpack caption') ident
  where
    getClasses (Image (_, classes, _) _ _) = classes
    getClasses _ = []
placeFigure x = return x

{-
-- | This function traverses the AST and replaces all images
placeFigure :: Block -> PandocIO Block
placeFigure (Figure (ident, _, _) (Caption _ caption) fig) = do
    caption' <- writeLaTeX def $ Pandoc (Meta M.empty) caption
    fig' <- writeLaTeX def $ Pandoc (Meta M.empty) fig
    return $ RawBlock "tex" $ T.pack $ printf
        ( "\\begin{figure*}[b!]"
        <> "\\centering"
        <> "%s"
        <> "\\caption{Continued on the following page}"
        <> "\\label{%s}"
        <> "\\end{figure*}"
        <> "\\addtocounter{figure}{-1}"
        <> "\\begin{figure*}[t!]"
        <> "\\caption{%s}"
        <> "\\end{figure*}"
        ) (T.unpack fig') ident (T.unpack caption')
placeFigure x = return x
-}
-}