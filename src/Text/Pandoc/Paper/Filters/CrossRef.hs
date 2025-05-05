{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Paper.Filters.CrossRef
    ( crossref
    ) where

import qualified Control.Monad.Reader  as R
import Control.Monad.State (runState)
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk

import Text.Pandoc.Paper.Filters.CrossRef.Types
import Text.Pandoc.Paper.Filters.CrossRef.Block
import Text.Pandoc.Paper.Filters.CrossRef.Reference

-- | This function traverses the AST and add cross references for figures and tables
crossref :: Pandoc -> Pandoc
crossref p@(Pandoc meta blks) = case nSupp of
    0 -> Pandoc meta' blk'
    _ -> let revBlk = reverse blk'
             supp = reverse $ take nSupp revBlk
             rest = reverse $ drop nSupp revBlk
          in Pandoc (setMeta "supplement" (MetaBlocks supp) meta') rest
  where
    Pandoc meta' blk' = R.runReader (crossRefAction doc) env
    (nSupp, doc) = case lookupMeta "supplement" meta of
        Just (MetaBlocks supplement) -> (length supplement, Pandoc meta $ blks <> supplement)
        Nothing -> (0, Pandoc meta blks)
    env = CrossRefEnv {
            creSettings = meta
          , creOptions = def
         }

-- | Enviromnent for 'CrossRefM'
data CrossRefEnv = CrossRefEnv {
                      creSettings :: Meta -- ^Metadata settings
                    , creOptions :: Options -- ^Internal pandoc-crossref options
                   }

-- | Essentially a reader monad for basic pandoc-crossref environment
type CrossRefM a = R.Reader CrossRefEnv a

crossRefAction :: Pandoc -> CrossRefM Pandoc
crossRefAction (Pandoc meta bs) = do
    bs' <- crossRefBlocks bs
    return $ Pandoc meta bs'

crossRefBlocks :: [Block] -> CrossRefM [Block]
crossRefBlocks blocks = do
  opts <- R.asks creOptions
  let
    doWalk = walkM replaceBlock blocks
      >>= bottomUpM (replaceRefs (refTypes opts))
    (result, st) = flip runState def $ flip R.runReaderT opts $ runWS doWalk
  st `seq` return result