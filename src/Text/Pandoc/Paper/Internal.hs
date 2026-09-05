{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Paper.Internal where

import Data.FileEmbed
import qualified Data.ByteString.Char8 as B

latexTemplateFile :: B.ByteString
latexTemplateFile = $(embedFile "data/default.tex")

docxTemplateFile :: B.ByteString
docxTemplateFile = $(embedFile "data/default.dotx")

htmlTemplateFile :: B.ByteString
htmlTemplateFile = $(embedFile "data/default.html")

cslNature :: B.ByteString
cslNature = $(embedFile "data/nature.csl")

cslCell :: B.ByteString
cslCell = $(embedFile "data/cell.csl")

cslAPA :: B.ByteString
cslAPA = $(embedFile "data/apa.csl")

template :: [(FilePath, B.ByteString)]
template = ("AGENTS.md", agentsTemplate) : filter ((/= "AGENTS.md") . fst) $(embedDir "data/template")

agentsTemplate :: B.ByteString
agentsTemplate = $(embedFile "data/template/AGENTS.md")
