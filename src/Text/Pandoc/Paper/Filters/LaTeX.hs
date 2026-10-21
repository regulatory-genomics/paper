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
import Text.Pandoc.Definition
import Text.Printf
import Text.Pandoc.Builder
import Data.Maybe

filterLaTeX :: Pandoc -> PandocIO Pandoc
filterLaTeX doc@(Pandoc meta _) = do
    let draft = case lookupMeta "draft" meta of
            Just (MetaBool b) -> b
            _ -> False
    if draft
        then walkM (placeFigure True) doc
        else walkM (placeFigure False . tableToLaTeX) doc

-- | This function traverses the AST and replaces all images
placeFigure :: Bool -> Block -> PandocIO Block
placeFigure draft (Figure (ident, _, _) (Caption _ caption) fig) = do
    let classes = query getClasses fig
    caption' <- writeLaTeX def $ Pandoc (Meta M.empty) caption
    fig' <- writeLaTeX def $ Pandoc (Meta M.empty) fig
    let (env, opt) = if draft then ("figure", "") else ("figure*", "[t]")
    return $ RawBlock "tex" $ T.pack $
        if "legend-next-page" `elem` classes
            then printf
                ( "\\begin{%s}[p!]\\centering"
                <> "%s"
                <> "\\end{%s}"
                <> "\\begin{%s}[t!]"
                <> "\\caption{%s}"
                <> "\\label{%s}"
                <> "\\end{%s}"
                ) env (T.unpack fig') env env (T.unpack caption') ident env
            else if "side-caption" `elem` classes
                then printf
                    ( "\\begin{SC%s}[1]\\begin{wide}"
                    <> "%s"
                    <> "\\caption{%s}"
                    <> "\\label{%s}"
                    <> "\\end{wide}\\end{SC%s}"
                    ) env (T.unpack fig') (T.unpack caption') ident env
                else printf
                    ( "\\begin{%s}%s"
                    <> "\\centering"
                    <> "%s"
                    <> "\\caption{%s}"
                    <> "\\label{%s}"
                    <> "\\end{%s}"
                    ) env opt (T.unpack fig') (T.unpack caption') ident env
  where
    getClasses (Image (_, classes, _) _ _) = classes
    getClasses _ = []
placeFigure _ x = return x

tableToLaTeX :: Block -> Block
tableToLaTeX (Table _ (Caption _ caption) _ (TableHead _ thead) tbodies (TableFoot _ tfoot)) =
    let txt = T.unlines
            [ "\\begin{table*}"
            , "\\centering"
            , "\\caption{" <> caption' <> "}"
            , "\\begin{tabular}{|" <> T.intercalate "|" (replicate ncol "l") <> "|}"
            , "\\hline"
            , header
            , "\\hline"
            , body
            , "\\hline"
            , footer
            , "\\hline"
            , "\\end{tabular}"
            , "\\end{table*}"
            ]
    in RawBlock (Format "latex") txt
  where
    ncol = let Row _ cells = head thead in length cells
    header = T.unlines $ map rowToLaTeX thead
    body = T.unlines $ concatMap (\(TableBody _ _ _ rows) -> map rowToLaTeX rows) tbodies
    footer = T.unlines $ map rowToLaTeX tfoot
    caption' = case runPure (writeLaTeX def $ Pandoc (Meta M.empty) caption) of
        Left err -> error $ show err
        Right x -> x
tableToLaTeX x = x

rowToLaTeX :: Row -> T.Text
rowToLaTeX (Row _ cells) = T.intercalate " & " (map cellToLaTeX cells) <> " \\\\"

cellToLaTeX :: Cell -> T.Text
cellToLaTeX (Cell _ _ _ _ blks) = case runPure (writeLaTeX def $ Pandoc (Meta M.empty) blks) of
    Left err -> error $ show err
    Right x -> x

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
