{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Paper.Filters.Authors (addAuthors) where

import Data.List (nub, sort, sortBy, intersperse)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Aeson
import Data.Maybe
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Builder
import qualified Data.Map as M

data Author = Author {
    name :: T.Text,
    email :: Maybe T.Text,
    affiliations :: [T.Text],
    equal_contribution :: Bool,
    corresponding :: Bool,
    marks :: [T.Text]
} deriving (Show)

instance ToMetaValue Author where
    toMetaValue author = MetaMap $ M.fromList [
        ("name", MetaString $ name author),
        ("email", MetaString $ fromMaybe "" $ email author),
        ("affiliations", MetaList $ map MetaString $ affiliations author),
        ("equal_contribution", MetaBool $ equal_contribution author),
        ("corresponding", MetaBool $ corresponding author),
        ("marks", MetaList $ map MetaString $ marks author)
        ]

data Affiliation = Affiliation {
    aff_mark :: T.Text,
    aff_name :: T.Text
} deriving (Show)

instance ToMetaValue Affiliation where
    toMetaValue aff = MetaMap $ M.fromList [
        ("mark", MetaString $ aff_mark aff),
        ("name", MetaString $ aff_name aff)
        ]

-- | Add author metadata to Pandoc.
addAuthors :: Format -> Pandoc -> Pandoc
addAuthors format (Pandoc meta blocks) = Pandoc meta' blocks
  where
    meta' = case format of
        "latex" -> setMeta "author" authors $
            setMeta "affiliation" affs $
            setEqualContribution meta
        _ -> setMeta "author" authorBlock meta
    (authors, affs) = addMarks $ readAuthors meta
    authorBlock =
        let authors' = para $ combineAuthors $ map formatAuthor authors
            affs' = para $ mconcat (intersperse linebreak (map formatAffliation affs))
                <> if any equal_contribution authors
                    then linebreak <> text "* These authors contributed equally"
                    else mempty
            corresponding_authors = map (\x -> name x <> " (" <> fromJust (email x) <> ")") $
                filter corresponding authors
            corres = para $ text $ "✉ Correspondence: " <> T.intercalate ", " corresponding_authors
        in [authors', affs', corres]

    formatAuthor :: Author -> Inlines
    formatAuthor author = 
        let m = marks author <> (if corresponding author then ["✉"] else [])
        in text (name author) <> superscript (text $ T.intercalate "," m)
    formatAffliation :: Affiliation -> Inlines
    formatAffliation aff = superscript (text $ aff_mark aff) <> text (aff_name aff)
    combineAuthors :: [Inlines] -> Inlines
    combineAuthors authors = mconcat $ intersperse spacer authors
      where
        spacer | length authors == 2 = text " and "
               | otherwise = text ", "

    setEqualContribution metadata
        | any equal_contribution authors = setMeta "equal-contribution"
            (MetaBool True) metadata
        | otherwise = metadata

addMarks :: [Author] -> ([Author], [Affiliation])
addMarks authors = (map addMark authors, map f $ sortBy (comparing snd) $ M.toList aff)
  where
    addMark author = author
        { marks = map (T.pack . show) (sort $ map (\x -> M.findWithDefault undefined x aff) $ affiliations author)
            <> (if equal_contribution author then ["*"] else [])
        }
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
