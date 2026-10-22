{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Paper.Filters.Unicode
    ( normalizeSuperSub
    ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Text.Pandoc.Definition
import Text.Pandoc.Walk

data Script = Super | Sub
    deriving (Eq)

-- | Mapping from Unicode superscript and subscript characters to their
-- ASCII base characters. Fonts commonly used for papers, such as TeX Gyre
-- Termes, lack glyphs for many of them (e.g. U+207B SUPERSCRIPT MINUS), so
-- leaving them as plain text makes the PDF renderer silently drop them.
table :: M.Map Char (Script, Char)
table = M.fromList $
    [ (c, (Super, b)) | (c, b) <- superscripts ]
        <> [ (c, (Sub, b)) | (c, b) <- subscripts ]
  where
    superscripts =
        [ ('\x2070', '0')
        , ('\x00b9', '1'), ('\x00b2', '2'), ('\x00b3', '3')
        , ('\x2074', '4'), ('\x2075', '5'), ('\x2076', '6')
        , ('\x2077', '7'), ('\x2078', '8'), ('\x2079', '9')
        , ('\x207a', '+'), ('\x207b', '-'), ('\x207c', '=')
        , ('\x207d', '('), ('\x207e', ')')
        , ('\x2071', 'i'), ('\x207f', 'n')
        ]
    subscripts =
        [ ('\x2080', '0'), ('\x2081', '1'), ('\x2082', '2'), ('\x2083', '3')
        , ('\x2084', '4'), ('\x2085', '5'), ('\x2086', '6'), ('\x2087', '7')
        , ('\x2088', '8'), ('\x2089', '9')
        , ('\x208a', '+'), ('\x208b', '-'), ('\x208c', '=')
        , ('\x208d', '('), ('\x208e', ')')
        ]

-- | Rewrite Unicode super/subscript characters in plain text as proper
-- Pandoc 'Superscript' and 'Subscript' inlines. Consecutive characters of
-- the same kind are grouped into a single element. Code, math and raw
-- blocks are left untouched.
normalizeSuperSub :: Pandoc -> Pandoc
normalizeSuperSub = walk (concatMap splitStr)

splitStr :: Inline -> [Inline]
splitStr (Str t) = map toInline $ T.groupBy sameKind t
  where
    toInline g = case T.uncons g of
        Nothing -> Str g
        Just (c, _) -> case M.lookup c table of
            Nothing -> Str g
            Just (Super, _) -> Superscript [Str (T.map base g)]
            Just (Sub, _) -> Subscript [Str (T.map base g)]
    sameKind x y = fmap fst (M.lookup x table) == fmap fst (M.lookup y table)
    base c = maybe c snd (M.lookup c table)
splitStr x = [x]
