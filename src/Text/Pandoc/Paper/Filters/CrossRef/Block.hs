{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Text.Pandoc.Paper.Filters.CrossRef.Block (replaceBlock) where

import Control.Monad (when)
import Control.Monad.Reader
import Control.Applicative
import Text.Pandoc.Walk (walk)
import Text.Read (readMaybe)
import qualified Data.Map as M
import Data.Maybe
import Data.List
import qualified Data.Text as T
import Lens.Micro.Mtl
import Text.Pandoc.Definition
import qualified Data.Sequence as S
import Data.Sequence (ViewR(..))
import Text.Pandoc.Shared (blocksToInlines)

import Text.Pandoc.Paper.Filters.CrossRef.Types

replaceBlock :: Block -> WS Block
replaceBlock blk = do
    opts <- ask :: WS Options
    let prefixes = flip mapMaybe (refTypes opts) $ \case
            RefCustom x -> Just $ x <> ":"
            _ -> Nothing
    replaceBlockHelper prefixes $ divBlocks blk
  where
    getRefType label prefixes
        | "fig:" `T.isPrefixOf` label = Just RefImage
        | "tbl:" `T.isPrefixOf` label = Just RefTable
        | otherwise = do
            (p, ps) <- uncons $ filter (\x -> x `T.isPrefixOf` label) prefixes
            if null ps
                then Just $ RefCustom $ T.dropEnd 1 p
                else Nothing
    replaceBlockHelper prefixes fig@(Figure (label, c, attrs) caption content)
        | Just refType <- getRefType label prefixes = do
            opts <- ask :: WS Options
            formattedIndex <- replaceAttr label attrs refType
            let prefix = titlePrefixFormatter opts refType formattedIndex
                caption' = addPrefixToCaption prefix caption
            return $ Figure (label, c, attrs) caption' content
        | otherwise = return fig
    replaceBlockHelper prefixes table@(Table (tattr, c, attrs) caption colspec header cells foot)
        | Just refType <- getRefType tattr prefixes = do
            opts <- ask :: WS Options
            formattedIndex <- replaceAttr tattr attrs refType
            let prefix = titlePrefixFormatter opts refType formattedIndex
                caption' = addPrefixToCaption prefix caption
            return $ Table (tattr, c, attrs) caption' colspec header cells foot
        | otherwise = return table
    replaceBlockHelper prefixes div@(Div attr@(label, _, _) [Table tattr (Caption short (btitle:rest)) colspec header cells foot])
        | Just refType <- getRefType label prefixes = do
            opts <- ask :: WS Options
            formattedIndex <- replaceAttr label [] refType
            let prefix = titlePrefixFormatter opts refType formattedIndex
                caption' = addPrefixToCaption prefix (Caption short (btitle:rest))
            return $ Div attr [Table tattr caption' colspec header cells foot]
        | otherwise = return div
    replaceBlockHelper _ x = return x

addPrefixToCaption :: [Inline] -> Caption -> Caption
addPrefixToCaption prefix (Caption short (b:blks)) = Caption short (b':blks)
    where b' = addPrefixToBlock prefix b

addPrefixToBlock :: [Inline] -> Block -> Block
addPrefixToBlock prefix block = case block of
    Para inlines -> Para (prefix ++ inlines)
    Plain inlines -> Plain (prefix ++ inlines)
    _ -> error "addPrefixToBlock: unsupported block type"

divBlocks :: Block -> Block
divBlocks (Table tattr (Caption short (btitle:rest)) colspec header cells foot)
  | not $ null title
  , Just label <- getRefLabel "tbl" [last title]
  = Div (label,[],[]) [
    Table tattr (Caption short $ walkReplaceInlines (dropWhileEnd isSpace (init title)) title btitle:rest) colspec header cells foot]
  where
    title = blocksToInlines [btitle]
    isSpace :: Inline -> Bool
    isSpace = (||) <$> (==Space) <*> (==SoftBreak)
divBlocks x = x

walkReplaceInlines :: [Inline] -> [Inline] -> Block -> Block
walkReplaceInlines newTitle title = walk replaceInlines
  where
  replaceInlines xs
    | xs == title = newTitle
    | otherwise = xs

-- | This function retrieves current index for a given reference and return the
-- formatted index. It also updates the counter in the state monad.
replaceAttr
    :: T.Text -- ^ Reference id
    -> [(T.Text, T.Text)] -- ^ Attributes
    -> RefType -- ^ Prefix type
    -> WS [Inline]
replaceAttr label attrs pfx = do
    let refLabel = lookup "label" attrs
        number = readMaybe . T.unpack =<< lookup "number" attrs
    Options{..} <- ask
    chap  <- S.take chaptersDepth <$> use (ctrsAt RefSection)
    prop' <- use $ refsAt pfx
    curIdx <- use $ ctrsAt pfx
    let i | Just n <- number = n
          | chap' :> last' <- S.viewr curIdx, chap' == chap = succ . fst $ last'
          | otherwise = 1
        index = chap S.|> (i, refLabel <|> customLabel ref i)
        ref = T.takeWhile (/=':') label
    when (M.member label prop') $
        error . T.unpack $ "Duplicate label: " <> label
    ctrsAt pfx .= index
    refsAt pfx %= M.insert label index
    return $ chapPrefix chapDelim index

getRefLabel :: T.Text -> [Inline] -> Maybe T.Text
getRefLabel _ [] = Nothing
getRefLabel tag ils
    | Str attr <- last ils
    , all (==Space) (init ils)
    , "}" `T.isSuffixOf` attr
    , ("{#"<>tag<>":") `T.isPrefixOf` attr
    = T.init `fmap` T.stripPrefix "{#" attr
getRefLabel _ _ = Nothing