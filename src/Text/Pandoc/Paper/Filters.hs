{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Paper.Filters
    ( citeproc
    , crossref
    , absPath
    , filterLaTeX
    , addAuthors
    , placeImagesAtEnd
    ) where

import Text.Pandoc.Paper.Filters.Citations (citeproc)
import Text.Pandoc.Paper.Filters.CrossRef (crossref)
import Text.Pandoc.Paper.Filters.Path (absPath)
import Text.Pandoc.Paper.Filters.LaTeX (filterLaTeX)
import Text.Pandoc.Paper.Filters.Authors (addAuthors)

import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Definition
import Text.Pandoc.Builder

placeImagesAtEnd :: Pandoc -> Pandoc
placeImagesAtEnd (Pandoc meta blks) = Pandoc meta' $ blks'
    <> toList (header 2 $ text "Figures") <> figs
    <> toList (header 2 $ text "Supplementary Figures") <> suppFigs
  where
    (meta', suppFigs) = collectFigures meta
    (blks', figs) = collectFigures blks
    collectFigures walkable = (walkable', figs)
      where
        walkable' = walk (filter f) walkable
          where
            f (Figure _ _ _) = False
            f _ = True
        figs = let f x@(Figure _ _ _) = [x]
                   f _ = []
                   g (Image _ _ _) = False
                   g _ = True
                in walk (filter g) $ query f walkable