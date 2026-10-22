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
    lead_contact :: Bool,
    marks :: [T.Text]
} deriving (Show)

instance ToMetaValue Author where
    toMetaValue author = MetaMap $ M.fromList [
        ("name", MetaString $ name author),
        ("email", MetaString $ fromMaybe "" $ email author),
        ("affiliations", MetaList $ map MetaString $ affiliations author),
        ("equal_contribution", MetaBool $ equal_contribution author),
        ("corresponding", MetaBool $ corresponding author),
        ("lead_contact", MetaBool $ lead_contact author),
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
            setMeta "lead-author" (leadAuthor authors) $
            setMeta "corresponding-email" (correspondingEmail authors) $
            setMeta "lead-contact" (leadContactLine hasLead leadNo) $
            setMeta "equal-contribution-note" (equalContributionNote equalMark) $
            meta
        "docx" -> setMeta "author" [authors'] $
            setMeta "author-notes" authorNotes meta
        _ -> setMeta "author" authorBlock meta
    (authors, affs, equalMark, leadNo) = addMarks $ readAuthors meta
    hasLead = any lead_contact authors
    authors' = para $ combineAuthors $ map formatAuthor authors
    affilsInlines = mconcat (intersperse linebreak (map formatAffliation affs))
        <> maybe mempty (\m -> linebreak <> superscript (text m)
                            <> text "These authors contributed equally") equalMark
    leadNote | hasLead = Just $ superscript (text $ T.pack (show leadNo)) <> text "Lead contact"
             | otherwise = Nothing
    corresNote = Just $ text $
        "✉ Correspondence: " <> T.intercalate ", " corresponding_authors
    corresponding_authors = map (\x -> name x <> " (" <> fromJust (email x) <> ")") $
        filter corresponding authors
    -- DOCX: the institute block is rendered by the bundled OpenXML template
    -- below the author names. The Div style is left-aligned in the bundled
    -- reference document; pandoc creates a fallback style for other documents.
    authorNotes = map (divWith ("", [], [("custom-style", "Author Note")]) . para) instituteNotes
    -- HTML: the whole institute block is wrapped in one Div so the template
    -- style sheet can align it as a unit.
    authorBlock =
        [ authors'
        , divWith ("", ["affiliations"], []) $ mconcat $ map para instituteNotes
        ]
    instituteNotes = affilsNote ++ catMaybes [leadNote, corresNote]
      where
        affilsNote | null affs && isNothing equalMark = []
                   | otherwise = [affilsInlines]

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

-- | Note for the lead contact: superscript mark followed by the label.
leadContactLine :: Bool -> Int -> MetaValue
leadContactLine False _ = MetaInlines []
leadContactLine True n  = MetaInlines
    [ Superscript [Str (T.pack (show n))]
    , Str "Lead contact"
    ]

-- | Note for equal contribution: superscript mark followed by the label.
equalContributionNote :: Maybe T.Text -> MetaValue
equalContributionNote Nothing  = MetaInlines []
equalContributionNote (Just m) = MetaInlines
    [ Superscript [Str m]
    , Str "These authors contributed equally"
    ]

-- | Lead author surname: last word of the first author's name.
leadAuthor :: [Author] -> T.Text
leadAuthor []    = ""
leadAuthor (a:_) = last (T.words (name a))

-- | Comma-separated emails of corresponding authors.
correspondingEmail :: [Author] -> T.Text
correspondingEmail as = T.intercalate ", " (map (fromMaybe "") (map email (filter corresponding as)))

-- | Assign author marks. Affiliation marks are numbered in author order,
-- followed by the shared equal-contribution mark and the lead-contact mark.
-- Returns the marked authors, the affiliation list, the shared
-- equal-contribution mark ('Nothing' when no author is an equal contributor),
-- and the lead-contact mark number.
addMarks :: [Author] -> ([Author], [Affiliation], Maybe T.Text, Int)
addMarks authors = (map addMark authors, map f $ sortBy (comparing snd) $ M.toList aff, equalMark, leadNo)  where
    addMark author = author
        { marks = map (T.pack . show) (sort $ map (\x -> M.findWithDefault undefined x aff) $ affiliations author)
            <> (if equal_contribution author then maybeToList equalMark else [])
            <> (if lead_contact author then [leadMark] else [])
        }
    aff = M.fromList $ zip (nub $ concatMap affiliations authors) [1..]
    nAffs = M.size aff
    hasEqual = any equal_contribution authors
    equalMark = if hasEqual then Just (T.pack $ show $ nAffs + 1) else Nothing
    leadNo = nAffs + (if hasEqual then 2 else 1)
    leadMark = T.pack $ show leadNo
    f (name, k) = Affiliation { aff_mark = T.pack $ show k, aff_name = name }

-- | Read author metadata from Pandoc and return a list of authors.
readAuthors :: Meta -> [Author]
readAuthors meta = case lookupMeta "author" meta of
    Just (MetaList authors) -> checkLeadContact $ map readAuthor authors
    _                       -> []

-- | There can be at most one lead-contact author.
checkLeadContact :: [Author] -> [Author]
checkLeadContact authors
    | count > 1 = error $ "At most one author can be marked as lead-contact, found " <> show count
    | otherwise = authors
  where
    count = length (filter lead_contact authors)

readAuthor :: MetaValue -> Author
readAuthor (MetaMap m) = Author {
        name = stringify $ M.findWithDefault undefined "name" m,
        email = stringify <$> M.lookup "email" m,
        affiliations = let MetaList x = M.findWithDefault (MetaList []) "affiliations" m in map stringify x,
        equal_contribution = maybe False isTrue $ M.lookup "equal_contribution" m,
        corresponding = maybe False isTrue $ M.lookup "corresponding" m,
        lead_contact = maybe False isTrue $ M.lookup "lead-contact" m,
        marks = let MetaList x = M.findWithDefault (MetaList []) "marks" m in map stringify x
    }
  where
    isTrue (MetaBool True) = True
    isTrue _               = False
