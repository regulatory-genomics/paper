{-# LANGUAGE OverloadedStrings, LambdaCase, TemplateHaskell, GeneralizedNewtypeDeriving, RankNTypes, DataKinds, RecordWildCards #-}
module Text.Pandoc.Paper.Filters.CrossRef.Reference where

import Text.Pandoc.Definition
import Text.Pandoc.Builder hiding ((<>))
import Control.Monad.Reader (ask)

import Control.Arrow as A
import Data.Function
import Data.List
import qualified Data.List.HT as HT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))
import Control.Monad (liftM2, join)

import Debug.Trace
import Lens.Micro.Mtl

import Text.Pandoc.Paper.Filters.CrossRef.Types

replaceRefs :: [RefType] -> [Inline] -> WS [Inline]
replaceRefs refTypes (Cite cits _:xs) = do
    opts <- ask :: WS Options
    toList . (<> fromList xs) . intercalate' (text ", ") . map fromList <$>
        mapM (replaceRefs' opts) (groupBy eqPrefix cits)
  where
    eqPrefix a b = uncurry (==) $
        (fmap uncapitalizeFirst . getLabelPrefix refTypes . citationId) <***> (a,b)
    (<***>) = join (***)
    replaceRefs' :: Options -> [Citation] -> WS [Inline]
    replaceRefs' opts cits'
        | Just prefix <- allCitsPrefix refTypes cits' = replaceRefs'' opts prefix cits'
        | otherwise = return [Cite cits' il']
      where
          il' = toList $
              str "["
            <> intercalate' (text "; ") (map citationToInlines cits')
            <> str "]"
          citationToInlines c =
            fromList (citationPrefix c) <> text ("@" <> citationId c)
              <> fromList (citationSuffix c)
    replaceRefs'' :: Options -> RefType -> [Citation] -> WS [Inline]
    replaceRefs'' opts = ($ opts) . flip $ replaceRefsOther refTypes
replaceRefs _ x = return x

replaceRefsOther :: [RefType] -> RefType -> Options -> [Citation] -> WS [Inline]
replaceRefsOther refTypes prefix opts cits = toList . intercalate' (text ", ") . map fromList <$>
    mapM (replaceRefsOther' refTypes prefix opts) (groupBy citationGroupPred cits)

citationGroupPred :: Citation -> Citation -> Bool
citationGroupPred = (==) `on` liftM2 (,) citationPrefix citationMode

replaceRefsOther' :: [RefType] -> RefType -> Options -> [Citation] -> WS [Inline]
replaceRefsOther' refTypes prefix opts cits@(firstCitation:_) = do
  indices <- mapM (getRefIndex prefix opts) cits
  let cap = maybe False isFirstUpper $ getLabelPrefix refTypes . citationId $ firstCitation
      writePrefix | all ((==SuppressAuthor) . citationMode) cits = id
                  | all (null . citationPrefix) cits = cmap $
                      refPrefixFormatter opts prefix cap
                  | otherwise = cmap $ toList . ((fromList (citationPrefix firstCitation) <> space) <>) . fromList
      cmap f [Link attr t w] | nameInLink opts = [Link attr (f t) w]
      cmap f x = f x
  return $ writePrefix (makeIndices opts indices)
replaceRefsOther' _ _ _ [] = return []

allCitsPrefix :: [RefType] -> [Citation] -> Maybe RefType
allCitsPrefix refTypes cits = find isCitationPrefix refTypes
  where
    isCitationPrefix p =
        all ((refType2Text p `T.isPrefixOf`) . uncapitalizeFirst . citationId) cits

refType2Text :: RefType -> T.Text
refType2Text = \case
    RefImage -> "fig:"
    RefTable -> "tbl:"
    RefSection -> "sec:"
    RefCustom x -> x <> ":"

getLabelPrefix :: [RefType] -> T.Text -> Maybe T.Text
getLabelPrefix refTypes lab
    | Just pfx <- pfxMap (uncapitalizeFirst p), pfx `elem` refTypes = Just $ p <> ":"
    | otherwise = Nothing
  where
    p = T.takeWhile (/=':') lab
    pfxMap = \case
        "fig" -> Just RefImage
        "tbl" -> Just RefTable
        "sec" -> Just RefSection
        "" -> Nothing
        x -> Just $ RefCustom x

getLabelWithoutPrefix :: T.Text -> T.Text
getLabelWithoutPrefix = T.drop 1 . T.dropWhile (/=':')

data RefData = RefData { rdLabel :: T.Text
                       , rdIdx :: Maybe Index
                       , rdSuffix :: [Inline]
                       , rdPfx :: T.Text
                       } deriving (Eq)

instance Ord RefData where
  (<=) = (<=) `on` rdIdx

getRefIndex :: RefType -> Options -> Citation -> WS RefData
getRefIndex prefix _opts Citation{citationId=cid,citationSuffix=suf} = do
    idx <- M.lookup lab <$> use (refsAt prefix)
    return RefData
        { rdLabel = lab
        , rdIdx = idx
        , rdSuffix = suf
        , rdPfx = refType2Text prefix }
  where
    lab = refType2Text prefix <> getLabelWithoutPrefix cid

data RefItem = RefRange RefData RefData | RefSingle RefData

makeIndices :: Options -> [RefData] -> [Inline]
makeIndices o s = format $ concatMap f $ HT.groupBy g $ sort $ nub s
  where
  g :: RefData -> RefData -> Bool
  g a b = all (null . rdSuffix) [a, b] && Just True == (liftM2 follows `on` rdIdx) b a 
  follows :: Index -> Index -> Bool
  follows a b
    | ai :> al <- S.viewr a
    , bi :> bl <- S.viewr b
    = ai == bi && A.first succ bl == al
    | otherwise = False
  f :: [RefData] -> [RefItem]
  f []  = []                          -- drop empty lists
  f [w] = [RefSingle w]                   -- single value
  f [w1,w2] = [RefSingle w1, RefSingle w2] -- two values
  f (x:xs) = [RefRange x (last xs)] -- shorten more than two values
  format :: [RefItem] -> [Inline]
  format [] = []
  format [x] = toList $ show'' x
  format [x, y] = toList $ show'' x <> fromList (pairDelim o) <> show'' y
  format xs = toList $ intercalate' (fromList $ refDelim o) init' <> fromList (lastDelim o) <> last'
    where initlast []     = error "emtpy list in initlast"
          initlast [y]    = ([], y)
          initlast (y:ys) = first (y:) $ initlast ys
          (init', last') = initlast $ map show'' xs
  show'' :: RefItem -> Inlines
  show'' (RefSingle x) = show' x
  show'' (RefRange x y) = show' x <> fromList (rangeDelim o) <> show' y
  show' :: RefData -> Inlines
  show' RefData{rdLabel=l, rdIdx=Just i}
      | linkReferences o = link ('#' `T.cons` l) "" (fromList txt)
      | otherwise = fromList txt
    where
      txt = chapPrefix (chapDelim o) i
  show' RefData{rdLabel=l, rdIdx=Nothing, rdSuffix = suf} =
    trace (T.unpack $ "Undefined cross-reference: " <> l)
          (strong (text $ "¿" <> l <> "?") <> fromList suf)
