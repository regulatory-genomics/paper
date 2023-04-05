{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Text.Pandoc.Paper.CSL
    ( cslNature
    , withStyle
    ) where

import Citeproc
import qualified Data.Map as M
import Control.Monad.Catch (MonadMask)
import Text.Pandoc
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Functor.Identity (runIdentity)
import System.IO.Temp (withSystemTempFile)
import System.IO

import Text.Pandoc.Paper.Internal (cslNature)

withStyle :: (MonadIO m, MonadMask m)
          => B.ByteString -> Pandoc -> (Pandoc -> m a) -> m a
withStyle style (Pandoc (Meta meta) blk) f = withSystemTempFile "tmp.csl" $ \fl h -> do
    liftIO $ B.hPutStr h style >> hClose h
    let meta' = Meta $ M.insert "csl" (MetaString $ T.pack fl) meta
    f $ Pandoc meta' blk