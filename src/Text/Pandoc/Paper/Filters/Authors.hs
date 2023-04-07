{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.Pandoc.Paper.Filters.Authors (addAuthors) where

import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Aeson
import Data.Maybe
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Builder (Inlines, text, ToMetaValue(..), setMeta)
import qualified Data.Map as M
import Debug.Trace
import GHC.Generics (Generic)

data Author = Author {
    name :: T.Text,
    email :: Maybe T.Text,
    affiliations :: [T.Text],
    equal_contribution :: Bool,
    corresponding :: Bool,
    marks :: [T.Text]
} deriving (Show, Generic)

instance ToMetaValue Author where
    toMetaValue author = MetaMap $ M.fromList [
        ("name", MetaString $ name author),
        ("email", MetaString $ fromMaybe "" $ email author),
        ("affiliations", MetaList $ map MetaString $ affiliations author),
        ("equal_contribution", MetaBool $ equal_contribution author),
        ("corresponding", MetaBool $ corresponding author),
        ("marks", MetaList $ map MetaString $ marks author)
        ]

instance ToJSON Author

data Affiliation = Affiliation {
    aff_mark :: T.Text,
    aff_name :: T.Text
} deriving (Show, Generic)

instance ToMetaValue Affiliation where
    toMetaValue aff = MetaMap $ M.fromList [
        ("mark", MetaString $ aff_mark aff),
        ("name", MetaString $ aff_name aff)
        ]

instance ToJSON Affiliation

addAuthors :: Pandoc -> Pandoc
addAuthors (Pandoc meta blocks) = Pandoc meta' blocks
  where
    meta' = setMeta "author" authors $
        setMeta "affiliation" affs $
        meta
    (authors, affs) = addMarks $ readAuthors meta

addMarks :: [Author] -> ([Author], [Affiliation])
addMarks authors = (map addMark authors, map f $ sortBy (comparing snd) $ M.toList aff)
  where
    addMark author = author { marks = map (T.pack . show) $ sort $ map (\x -> M.findWithDefault undefined x aff) $ affiliations author }
    aff = M.fromList $ zip (nub $ concatMap affiliations authors) [1..]
    f (name, k) = Affiliation { aff_mark = T.pack $ show k, aff_name = name }

-- | Read author metadata from Pandoc and return a list of authors.
readAuthors :: Meta -> [Author]
readAuthors meta = case lookupMeta "author" meta of
    Just (MetaList authors) -> map readAuthor authors
    _                       -> []

readAuthor :: MetaValue -> Author
readAuthor (MetaMap m) = Author {
        name = stringify $ M.findWithDefault undefined "name" m,
        email = stringify <$> M.lookup "email" m,
        affiliations = let MetaList x = M.findWithDefault (MetaList []) "affiliations" m in map stringify x,
        equal_contribution = maybe False isTrue $ M.lookup "equal_contribution" m,
        corresponding = maybe False isTrue $ M.lookup "corresponding" m,
        marks = let MetaList x = M.findWithDefault (MetaList []) "marks" m in map stringify x
    }
  where
    isTrue (MetaBool True) = True
    isTrue _               = False
