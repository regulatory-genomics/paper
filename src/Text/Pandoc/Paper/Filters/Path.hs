{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Paper.Filters.Path
    ( absPath
    ) where

import           Control.Monad
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import           Data.List
import           System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Walk (walkM)
import System.Directory (makeAbsolute, doesFileExist)

-- | This function changes the relative paths of images to absolute paths
absPath :: Pandoc -> PandocIO Pandoc
absPath = walkM (liftIO . updateInline)

updateInline :: Inline -> IO Inline
updateInline (Image attr alt (url, title)) = do
  absUrl <- makeAbsolute' url
  return $ Image attr alt (absUrl, title)
updateInline (Link attr inlines (url, title)) = do
  absUrl <- makeAbsolute' url
  return $ Link attr inlines (absUrl, title)
updateInline inline = return inline

makeAbsolute' :: T.Text -> IO T.Text
makeAbsolute' path = do
    exist <- doesFileExist $ T.unpack path
    if exist
        then T.pack <$> makeAbsolute (T.unpack path)
        else return path