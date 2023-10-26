{-# LANGUAGE OverloadedStrings, TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes, DataKinds, RecordWildCards #-}
module Text.Pandoc.Paper.Filters.CrossRef.Types where

import Control.Monad.State
import Control.Monad.Reader
import Data.Char (isUpper, toLower, toUpper)

import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro.GHC
import Lens.Micro.TH
import Text.Pandoc.Definition
import qualified Data.Sequence as S
import Text.Pandoc.Builder hiding ((<>))

-- | The monad for cross reference
newtype WS a = WS { runWS :: ReaderT Options (State References) a }
    deriving (Functor, Applicative, Monad, MonadReader Options, MonadState References)

data Options = Options { refTypes :: [RefType]
                       , chaptersDepth   :: Int
                       , chapDelim   :: [Inline]
                       , pairDelim :: [Inline]
                       , lastDelim :: [Inline]
                       , refDelim :: [Inline]
                       , rangeDelim :: [Inline]
                       , customLabel :: Text -> Int -> Maybe Text
                       , nameInLink :: Bool
                       , linkReferences :: Bool
                       , refPrefixFormatter :: RefType -> Bool -> [Inline] -> [Inline]
                       , titlePrefixFormatter :: RefType -> [Inline] -> [Inline]
                       }

defaultRefPrefixFormatter :: RefType -> Bool -> [Inline] -> [Inline]
defaultRefPrefixFormatter RefImage True i = [Str "Fig.", Space] <> i
defaultRefPrefixFormatter RefImage False i = [Str "figure", Space] <> i
defaultRefPrefixFormatter RefTable True i = [Str "Table", Space] <> i
defaultRefPrefixFormatter RefTable False i = [Str "table", Space] <> i
defaultRefPrefixFormatter RefSection True i = [Str "Section", Space] <> i
defaultRefPrefixFormatter RefSection False i = [Str "section", Space] <> i
defaultRefPrefixFormatter (RefCustom "supp_fig") _ i = [Str "Supplementary Fig.", Space] <> i
defaultRefPrefixFormatter (RefCustom "ext_fig") _ i = [Str "Extended Data Fig.", Space] <> i
defaultRefPrefixFormatter (RefCustom t) _ i = [Str t, Space] <> i

defaultTitlePrefixFormatter :: RefType -> [Inline] -> [Inline]
defaultTitlePrefixFormatter RefImage i = toList $ strong $ text "Fig. " <> fromList i <> text " | "
defaultTitlePrefixFormatter RefTable i = toList $ strong $ text "Table " <> fromList i <> text " | "
defaultTitlePrefixFormatter RefSection i = toList $ strong $ text "Section " <> fromList i <> text " | "
defaultTitlePrefixFormatter (RefCustom "supp_fig") i = toList $ strong $ text "Supplementary Fig. " <> fromList i <> text " | "
defaultTitlePrefixFormatter (RefCustom "ext_fig") i = toList $ strong $ text "Extended Data Fig. " <> fromList i <> text " | "
defaultTitlePrefixFormatter (RefCustom t) i = toList $ strong $ text t <> text " | "
  
instance Default Options where
    def = Options
        { refTypes = [RefImage, RefTable, RefSection, RefCustom "supp_fig", RefCustom "ext_fig"]
        , chaptersDepth = 1
        , chapDelim = [Str "."]
        , pairDelim = [Str ",", Space]
        , lastDelim = [Str ",", Space]
        , refDelim = [Str ",", Space]
        , rangeDelim = [Str "-"]
        , customLabel = \_ _ -> Nothing
        , nameInLink = False
        , linkReferences = True
        , refPrefixFormatter = defaultRefPrefixFormatter
        , titlePrefixFormatter = defaultTitlePrefixFormatter
        }

-- | Index of a reference at different levels, chapter, section, subsection, etc. 
type Index = S.Seq (Int, Maybe Text)

data RefType = RefImage
             | RefTable
             | RefSection
             | RefCustom Text
             deriving (Eq, Ord, Show)

type RefMap = M.Map Text Index

-- | The state of the cross reference, containing the reference map and the counter
data References = References { _stRefs :: M.Map RefType RefMap
                             , _stCtrs :: M.Map RefType Index
                             } deriving (Show, Eq)

instance Default References where
  def = References mempty mempty

makeLenses ''References

-- | Retrieve the reference map for a given prefix
refsAt :: RefType -> Lens' References RefMap
refsAt pfx = stRefs . at pfx . non mempty

-- | Retrieve the counter for a given prefix
ctrsAt :: RefType -> Lens' References Index
ctrsAt pfx = stCtrs . at pfx . non mempty

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

chapPrefix :: [Inline] -> Index -> [Inline]
chapPrefix delim = toList
  . intercalate' (fromList delim)
  . fmap str
  . S.filter (not . T.null)
  . fmap (uncurry (fromMaybe . T.pack . show))

intercalate' :: (Eq a, Monoid a, Foldable f) => a -> f a -> a
intercalate' s xs
  | null xs = mempty
  | otherwise = foldr1 (\x acc -> x <> s <> acc) xs

capitalizeFirst :: T.Text -> T.Text
capitalizeFirst t
  | Just (x, xs) <- T.uncons t = toUpper x `T.cons` xs
  | otherwise = T.empty

uncapitalizeFirst :: T.Text -> T.Text
uncapitalizeFirst t
  | Just (x, xs) <- T.uncons t = toLower x `T.cons` xs
  | otherwise = T.empty

isFirstUpper :: T.Text -> Bool
isFirstUpper xs
  | Just (x, _) <- T.uncons xs  = isUpper x
  | otherwise = False
