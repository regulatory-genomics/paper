{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Paper.Filters.Citations
    ( citeproc
    ) where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (forM)
import System.Directory (doesFileExist)
import System.IO
import System.IO.Temp (withSystemTempFile)
import Network.HTTP.Client (parseRequest, httpLbs, responseBody, requestHeaders, responseStatus)
import Network.HTTP.Types.Status (Status, ok200, statusMessage)
import Network.HTTP.Client.TLS (newTlsManager)
import Text.Pandoc.Readers.BibTeX (readBibTeX)
import Text.BibTeX.Entry (identifier)
import Control.Monad.IO.Class
import qualified Text.BibTeX.Parse as P
import qualified Text.BibTeX.Format as F
import Data.ByteString.Encoding (decode, utf8)
import Text.Parsec (parse)
import Text.Pandoc
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Builder
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Writers.BibTeX (writeBibTeX)
import Control.Monad.State.Lazy (State, modify, execState)
import Text.Pandoc.Definition
import Data.Either (rights)
import Text.Printf (printf)

import Text.Pandoc.Paper.CSL

data CitationID = DOI T.Text
                | PMID T.Text
                deriving (Eq, Ord, Show)

-- | Download a reference entry from the web.
-- Read BibTeX from an input string and return a Pandoc document. The document will have only metadata, with an empty body. The metadata will contain a references field with the bibliography entries, and a nocite field with the wildcard `[@*]`.
getReferences :: [CitationID] -> PandocIO Pandoc
getReferences ids = (T.unlines <$> mapM f ids) >>= readBibTeX def
  where
    f (DOI i) = liftIO (getBibTexByDoi i) >>= \case
        Left err -> error $ B.unpack err
        Right x -> return x
    f _ = undefined

-- | Process citations in a document. This will automatically download references
-- from the web and add them to the bibliography.
citeproc :: Maybe FilePath -- ^ Path to the bibliography file.
         -> Maybe B.ByteString
         -> Pandoc
         -> PandocIO Pandoc
citeproc refFl cslData doc = do
    Pandoc meta d <- case cslData of
        Nothing -> addCitations doc
        Just style -> withStyle style doc addCitations
    let meta' = setMeta "linkBibliography" (MetaBool False) meta
    return $ Pandoc meta' d
  where
    addCitations doc = do
        let refIDs = collectRefs doc
        case refFl of
            Just fl -> do
                fileExists <- liftIO $ doesFileExist fl
                idsToCollect <- if fileExists
                    then liftIO $ (refIDs `S.difference`) . S.fromList . getBibIds <$> readFile fl
                    else return refIDs
                when (S.size idsToCollect > 0) $ report $ Fetching $ T.pack $
                    printf "%d references from the web" (S.size idsToCollect)
                refs <- getReferences (S.toList idsToCollect) >>= writeBibTeX def
                liftIO $ withFile fl AppendMode $ \h -> T.hPutStr h refs
                processCitations $ placeRefsInMeta "refs" $ addBibliography fl doc
            Nothing -> do
                when (S.size refIDs > 0) $ report $ Fetching $ T.pack $
                    printf "%d references from the web" (S.size refIDs)
                refs <- getReferences (S.toList refIDs)
                processCitations $ placeRefsInMeta "refs" doc <> refs

-- | Put the references in the metadata using:
-- $refAnchor: |
--   # References
--   ::: {#refs}
--   :::
placeRefsInMeta :: T.Text -> Pandoc -> Pandoc
placeRefsInMeta refAnchor (Pandoc (Meta meta) blk) = Pandoc (Meta newMeta) blk
  where
    -- add unumbered header
    header = Header 1 ("", ["unnumbered"], []) [Str "References"]
    div = Div ("refs", [], []) []
    newMeta = M.insert refAnchor (MetaBlocks [header, div]) meta

addBibliography :: FilePath -> Pandoc -> Pandoc
addBibliography fl (Pandoc (Meta meta) blks) = Pandoc meta' blks
  where
    meta' = Meta $ M.insert "bibliography" (MetaString $ T.pack fl) meta

collectRefs :: Pandoc -> S.Set CitationID
collectRefs doc = execState (walkM f doc) S.empty
  where
    f x@(Cite cs _) = mapM_ g cs >> return x
    f x = return x
    g c | isRefId (citationId c) = modify (S.insert (DOI $ citationId c))
        | otherwise = return ()
    isRefId x = "doi:" `T.isPrefixOf` x
    
getBibTexByDoi :: T.Text -> IO (Either B.ByteString T.Text)
getBibTexByDoi doi = do
    manager <- newTlsManager
    request <- parseRequest url
    response <- httpLbs request{requestHeaders=[("Accept", "application/x-bibtex")]} manager
    let status = responseStatus response
    return $ if status == ok200
        then Right $ changeId doi $ decode utf8 $ BL.toStrict $ responseBody response
        else Left $ statusMessage status
  where
    url = baseUrl <> T.unpack (T.drop 4 doi)
    baseUrl = "https://doi.org/"

changeId :: T.Text -> T.Text -> T.Text
changeId newId input = case parse P.entry "bibtex" (T.unpack input) of
    Left err -> error $ show err
    Right r -> T.pack $ F.entry $ r{identifier=T.unpack newId}

-- | Get all bibliography ids from a bibtex file
getBibIds :: String -> [CitationID]
getBibIds input = case parse (P.skippingLeadingSpace P.file) "bibtex" input of
    Left err -> error $ show err
    Right r -> map (DOI . T.pack . identifier) r