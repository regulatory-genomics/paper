{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Paper.Internal where

import Citeproc
import Text.Pandoc (runWithPartials, PandocIO, Pandoc(..), Meta(..), MetaValue(..), Template, compileTemplate)
import Data.FileEmbed
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

contextTemplate :: B.ByteString
contextTemplate = $(embedFile "data/default.context")

latexTemplateFile :: B.ByteString
latexTemplateFile = $(embedFile "data/default.tex")

docxTemplateFile :: B.ByteString
docxTemplateFile = $(embedFile "data/default.dotx")

htmlTemplateFile :: B.ByteString
htmlTemplateFile = $(embedFile "data/default.html")

cvTemplate :: B.ByteString
cvTemplate = $(embedFile "data/cv.context")

cslNature :: B.ByteString
cslNature = $(embedFile "data/nature.csl")

cslCell :: B.ByteString
cslCell = $(embedFile "data/cell.csl")

cslAPA :: B.ByteString
cslAPA = $(embedFile "data/apa.csl")