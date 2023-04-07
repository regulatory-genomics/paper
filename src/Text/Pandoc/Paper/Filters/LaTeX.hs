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
            ( "\\begin{figure*}"
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

{-
tableToLaTeX :: PandocMonad m
             -> Ann.Table
             -> T.Text
tableToLaTeX (Ann.Table (ident, _, _) (Caption _ caption) specs thead tbodies tfoot) = do
    caption' <- writeLaTeX def $ Pandoc (Meta M.empty) caption

  CaptionDocs capt captNotes <- captionToLaTeX inlnsToLaTeX caption ident
  let removeNote (Note _) = Span ("", [], []) []
      removeNote x        = x
  let colCount = ColumnCount $ length specs
  -- The first head is not repeated on the following pages. If we were to just
  -- use a single head, without a separate first head, then the caption would be
  -- repeated on all pages that contain a part of the table. We avoid this by
  -- making the caption part of the first head. The downside is that we must
  -- duplicate the header rows for this.
  head' <- do
    let mkHead = headToLaTeX blksToLaTeX colCount
    case (not $ isEmpty capt, not $ isEmptyHead thead) of
      (False, False) -> return "\\toprule\\noalign{}"
      (False, True)  -> mkHead thead
      (True, False)  -> return (capt $$ "\\toprule\\noalign{}" $$ "\\endfirsthead")
      (True, True)   -> do
        -- avoid duplicate notes in head and firsthead:
        firsthead <- mkHead thead
        repeated  <- mkHead (walk removeNote thead)
        return $ capt $$ firsthead $$ "\\endfirsthead" $$ repeated
  rows' <- mapM (rowToLaTeX blksToLaTeX colCount BodyCell) $
                mconcat (map bodyRows tbodies)
  foot' <- if isEmptyFoot tfoot
           then pure empty
           else do
             lastfoot <- mapM (rowToLaTeX blksToLaTeX colCount BodyCell) $
                              footRows tfoot
             pure $ "\\midrule\\noalign{}" $$ vcat lastfoot
  modify $ \s -> s{ stTable = True }
  notes <- notesToLaTeX <$> gets stNotes
  beamer <- gets stBeamer
  return
    $  "\\begin{longtable}[]" <>
          braces ("@{}" <> colDescriptors tbl <> "@{}")
          -- the @{} removes extra space at beginning and end
    $$ head'
    $$ "\\endhead"
    $$ vcat
       -- Longtable is not able to detect pagebreaks in Beamer; this
       -- causes problems with the placement of the footer, so make
       -- footer and bottom rule part of the body when targeting Beamer.
       -- See issue #8638.
       (if beamer
             then [ vcat rows'
                  , foot'
                  , "\\bottomrule\\noalign{}"
                  ]
             else [ foot'
                  , "\\bottomrule\\noalign{}"
                  , "\\endlastfoot"
                  ,  vcat rows'
                  ])
    $$ "\\end{longtable}"
    $$ captNotes
    $$ notes
-}