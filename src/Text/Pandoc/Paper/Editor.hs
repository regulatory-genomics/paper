{-# LANGUAGE OverloadedStrings #-}

-- | Source-mapped editing support: report which top-level Markdown blocks can
-- be edited in place, and validate a single-block replacement without writing.
--
-- The mapping is deliberately conservative. A block is only offered when a
-- plain-text block scanner and Pandoc's own parse agree on the whole file's
-- block sequence; anything ambiguous makes the file uneditable instead of
-- risking a lossy rewrite. Protected inline syntax (citations, math, quotes,
-- raw TeX) is detected from the AST and surfaced as `inline: "rich"`, where the
-- view falls back to editing the exact source text of the block.
module Text.Pandoc.Paper.Editor
  ( inspectValue
  , planEditValue
  ) where

import Control.Exception (try)
import Data.Aeson (Object, Value (..), eitherDecode, object, toJSON, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Text.Pandoc
import Text.Pandoc.Paper.Readers (readDocText, readYamlWithSources)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query)

-- | One scanned top-level block, in UTF-8 byte offsets.
data Segment = Segment
  { segStart :: Int
  , segEnd :: Int
  , segKind :: T.Text
  , segOpaque :: Bool
  , segVisible :: Bool   -- ^ a definition Pandoc attaches rather than a block
  , segLine :: Int
  }

blockKind :: Block -> T.Text
blockKind = blockKindName

-- | Constructor name, without arguments. `show` keeps this working across
-- pandoc-types releases that add or reorder constructor fields.
ctorName :: Show a => a -> T.Text
ctorName = T.takeWhile (/= ' ') . T.pack . show

blockKindName :: Block -> T.Text
blockKindName block
  | name `elem` (["OrderedList", "BulletList"] :: [T.Text]) = "List"
  | name `elem` (["Plain", "Para"] :: [T.Text]) = "Paragraph"
  | name == "Header" = "Heading"
  | name == "RawBlock" = "Raw"
  | name == "CodeBlock" = "Code"
  | name == "BlockQuote" = "Quote"
  | name == "HorizontalRule" = "Rule"
  | otherwise = name
  where
    name = ctorName block

-- | Only text carrying no inline syntax can be edited as plain text.
plainInlines :: Block -> Bool
plainInlines block = all isPlain findings
  where
    findings = query (\(inline :: Inline) -> [ctorName inline]) block :: [T.Text]
    isPlain name = name `elem` (["Str", "Space", "SoftBreak", "LineBreak"] :: [T.Text])

editableKind :: T.Text -> Bool
editableKind kind = kind `elem` (["Heading", "Paragraph", "List"] :: [T.Text])

unBlocks :: Pandoc -> [Block]
unBlocks (Pandoc _ blocks) = blocks

readText' :: T.Text -> PandocIO Pandoc
readText' = readDocText

-- | Scan one file, then check the result against Pandoc's own parse.
inspectFile :: FilePath -> IO Value
inspectFile path = do
  raw <- BS.readFile path
  case TE.decodeUtf8' raw of
    Left _ -> pure (unmapped "the file is not valid UTF-8")
    Right text -> do
      parsed <- runIOorExplode (readText' text)
      let segments = scanLines text
          blocks = unBlocks parsed
          visible = filter segVisible segments
      pure $ case compareKinds visible blocks of
        Just why -> withFields (unmapped why)
          [ ("scannerKinds", toJSON (take 80 (map segKind visible)))
          , ("pandocKinds", toJSON (take 80 (map blockKindName blocks)))
          , ("scannerLines", toJSON (take 80 (map segLine visible)))
          ]
        Nothing -> object
          [ "mapped" .= True
          , "reason" .= (Nothing :: Maybe T.Text)
          , "blocks" .= [ blockValue text visible blocks i | i <- [0 .. length blocks - 1] ]
          ]
  where
    unmapped why = object
      [ "mapped" .= False
      , "reason" .= why
      , "blocks" .= ([] :: [Value])
      ]

blockValue :: T.Text -> [Segment] -> [Block] -> Int -> Value
blockValue text segments blocks i = object
  [ "index" .= i
  , "kind" .= kind
  , "line" .= segLine seg
  , "byteStart" .= segStart seg
  , "byteEnd" .= segEnd seg
  , "editable" .= (not (segOpaque seg) && editableKind kind)
  , "inline" .= (if editableKind kind && plainInlines block then "plain" else "rich" :: T.Text)
  , "text" .= T.unpack (stringify block :: T.Text)
  , "source" .= T.unpack (sliceText text seg)
  ]
  where
    seg = segments !! i
    block = blocks !! i
    kind = blockKindName block

compareKinds :: [Segment] -> [Block] -> Maybe T.Text
compareKinds segments blocks = go (0 :: Int) segments blocks
  where
    go _ [] [] = Nothing
    go i (seg : restSegs) (block : restBlocks)
      | compatible (segKind seg) (blockKindName block) = go (i + 1) restSegs restBlocks
      | otherwise = Just $ T.concat
          [ "block ", tshow (i + 1), " starting on line ", tshow (segLine seg)
          , " looks like ", segKind seg, " but Pandoc reads it as ", blockKindName block
          ]
    go i segs bs = Just $ T.concat
      [ "the block scanner found ", tshow (i + length segs)
      , " blocks, Pandoc found ", tshow (i + length bs)
      ]

-- | Pandoc upgrades some paragraph-looking text into its own block kind (a
-- lone image becomes a Figure, a simple table becomes Table). The byte range
-- is still the whole block, and these blocks are never offered as editable.
compatible :: T.Text -> T.Text -> Bool
compatible scanned pandoc
  | scanned == pandoc = True
  | scanned == "Paragraph" =
      pandoc `elem` (["Figure", "Table", "LineBlock", "DefinitionList"] :: [T.Text])
  | otherwise = False

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- | Byte offset of the first byte of every line, plus a final sentinel.
lineOffsets :: T.Text -> [Int]
lineOffsets text = scanl step 0 (map (BS.length . TE.encodeUtf8) (T.splitOn "\n" text))
  where
    step acc len = acc + len + 1

sliceText :: T.Text -> Segment -> T.Text
sliceText text seg = TE.decodeUtf8With (\_ _ -> Just '\xfffd')
  (BS.take (segEnd seg - segStart seg) (BS.drop (segStart seg) (TE.encodeUtf8 text)))

-- | Conservative scanner. It recognises the constructs the first release
-- supports and marks everything else opaque, so unmodified opaque blocks are
-- always written back byte-for-byte.
scanLines :: T.Text -> [Segment]
scanLines text = go 0
  where
    ls = T.splitOn "\n" text
    offsets = lineOffsets text
    total = BS.length (TE.encodeUtf8 text)
    count = length ls
    line k = T.dropWhileEnd (== '\r') (ls !! k)
    blank k = k >= count || T.null (T.strip (line k))
    startOf k = if k < length offsets then offsets !! k else total

    go k
      | k >= count = []
      | T.null (T.strip (line k)) = go (k + 1)
      | otherwise =
          let (kind, opaque, visible, endLine) = classify k
              seg = Segment (startOf k) (startOf (endLine + 1)) kind opaque visible (k + 1)
          in seg : go (endLine + 1)

    classify k
      | Just end <- fence k = ("Code", True, True, end)
      | Just end <- yamlBlock k = ("Metadata", True, False, end)
      | isHeading (line k) = ("Heading", False, True, k)
      | Just end <- setext k = ("Heading", True, True, end)
      | Just end <- multilineTable k = ("Table", True, True, end)
      | isRule (line k) = ("Rule", True, True, k)
      -- Pandoc has no math block: display math is a paragraph holding a Math
      -- inline, so the range still maps to a Paragraph and stays read-only.
      | Just end <- mathBlock k = ("Paragraph", True, True, end)
      | Just end <- divFence k = ("Div", True, True, end)
      | isRefDef (line k) = ("Reference", True, False, k)
      | isFootnoteDef (line k) = ("Footnote", True, False, continueUntilBlank k)
      | isGridTable (line k) = ("Table", True, True, continueWhile isGridTable k)
      | isTableCaption k = case nextContent (k + 1) of
          Just n -> ("Table", True, True, tableBody n)
          Nothing -> ("Paragraph", False, True, k)
      | isTableLine (line k) =
          let end = continueWhile isTableLine k
          in if any (isPipeTableLine . line) [k .. end]
               then ("Table", True, True, end)
               else ("LineBlock", True, True, end)
      -- Pandoc's native_divs extension parses a raw <div> block as a Div.
      | isHtmlDivOpen (line k) = ("Div", True, True, htmlDivEnd k)
      | isRawHtml (line k) = ("Raw", True, True, continueUntilBlank k)
      | isLatexMacro (line k) = ("Raw", True, True, k)
      | isBlockQuote (line k) = ("Quote", True, True, continueWhile isBlockQuote k)
      | Just end <- indentedCode k = ("Code", True, True, end)
      | isListStart (line k) = listSegment k
      | isDefinitionStart k = ("DefinitionList", True, True, definitionEnd k)
      | otherwise = ("Paragraph", False, True, continueUntilBlank k)

    -- "Term" followed by a line starting with ':' is a definition list. Pandoc
    -- keeps loose pairs in one block, so this consumes every following pair.
    isDefinitionStart k = case nextContent (k + 1) of
      Just n -> isDefBody (line n)
      Nothing -> False

    isDefBody l =
      let t = T.stripStart l
      in ":" `T.isPrefixOf` t && not ("::" `T.isPrefixOf` t)

    definitionEnd k = go k
      where
        go j = case nextContent (j + 1) of
          Nothing -> j
          Just n
            | isDefBody (line n) -> go (bodyEnd n)
            | otherwise -> case nextContent (n + 1) of
                Just m | isDefBody (line m) -> go (bodyEnd m)
                _ -> j
        bodyEnd n
          | n + 1 < count && not (blank (n + 1))
          , isDefBody (line (n + 1)) || isIndented (line (n + 1)) = bodyEnd (n + 1)
          | otherwise = n

    -- A list owns every following non-blank line: nested items, wrapped text
    -- and lazy continuations all belong to it until the blank line.
    -- Pandoc's table_captions extension attaches a `Table: ...` line to the
    -- table that follows it, so the caption is part of that block.
    isTableCaption k =
      let t = T.stripStart (line k)
      in "Table:" `T.isPrefixOf` t
         && case nextContent (k + 1) of
              Just n -> isTableLine (line n) || isGridTable (line n) || isDashLine (line n)
              Nothing -> False

    tableBody n
      | isGridTable (line n) = continueWhile isGridTableBody n
      | isTableLine (line n) = continueWhile isTableLine n
      | isDashLine (line n) = continueWhile (\l -> isDashLine l || not (T.null (T.strip l))) n
      | otherwise = n

    -- Pandoc's multiline table: a long dashes border, rows, closing border.
    multilineTable k
      | isDashLine (line k) =
          let end = continueUntilBlank k
          in if any (\j -> j > k && isDashLine (line j)) [k .. end]
               && any (\j -> not (isDashLine (line j))) [k .. end]
               then Just end
               else Nothing
      | otherwise = Nothing

    isHtmlDivOpen l =
      let t = T.toLower (T.stripStart l)
      in "<div" `T.isPrefixOf` t && (T.length t == 4 || T.index t 4 `elem` ("> " :: String))

    htmlDivEnd j
      | j + 1 >= count = j
      | blank (j + 1) = j
      | "</div" `T.isPrefixOf` T.toLower (T.stripStart (line (j + 1))) = j + 1
      | otherwise = htmlDivEnd (j + 1)

    isGridTableBody l =
      let t = T.stripStart l
      in "+" `T.isPrefixOf` t || "|" `T.isPrefixOf` t

    listSegment k =
      let end = continueList k
          flat = all (\j -> T.null (T.strip (line j))
                        || (isListStart (line j)
                            && not (T.isPrefixOf " " (line j))
                            && not (T.isPrefixOf "\t" (line j)))) [k .. end]
      in if flat && not ("(@" `T.isPrefixOf` T.stripStart (line k))
           then ("List", False, True, end)
           else ("List", True, True, end)

    -- A loose list keeps one block across the blank lines between its items.
    continueList j
      | j + 1 >= count = j
      | blank (j + 1) =
          case nextContent (j + 1) of
            Just n | isListStart (line n) && not (T.isPrefixOf " " (line n)) -> continueList n
            _ -> j
      | otherwise = continueList (j + 1)

    nextContent j
      | j >= count = Nothing
      | T.null (T.strip (line j)) = nextContent (j + 1)
      | otherwise = Just j

    continueUntilBlank k
      | k + 1 >= count = k
      | blank (k + 1) = k
      | startsBlock (k + 1) = k
      | otherwise = continueUntilBlank (k + 1)

    continueWhile p k
      | k + 1 >= count = k
      | blank (k + 1) = k
      | p (line (k + 1)) = continueWhile p (k + 1)
      | otherwise = k

    -- A paragraph yields only to constructs that also interrupt one in Pandoc
    -- here: headings and quotes require a preceding blank line (`blank_before_*`).
    startsBlock k =
      let l = line k
      in isFence l || isRawHtml l

    yamlBlock k
      | k == 0 && T.strip (line 0) == "---" =
          let end = search 1
          in if end < count then Just end else Nothing
      | otherwise = Nothing
      where
        search j
          | j >= count = count
          | T.strip (line j) `elem` (["---", "..."] :: [T.Text]) = j
          | otherwise = search (j + 1)

    fence k
      | isFence (line k) =
          let marker = T.take 3 (T.stripStart (line k))
          in Just (search (k + 1) marker)
      | otherwise = Nothing
      where
        search j marker
          | j >= count = count - 1
          | marker `T.isPrefixOf` T.stripStart (line j) = j
          | otherwise = search (j + 1) marker

    setext k
      | k + 1 < count && not (blank (k + 1)) && isSetextUnderline (line (k + 1)) = Just (k + 1)
      | otherwise = Nothing

    mathBlock k
      | "$$" `T.isPrefixOf` T.stripStart (line k) = Just (search (k + 1))
      | otherwise = Nothing
      where
        search j
          | j >= count = count - 1
          | "$$" `T.isSuffixOf` T.stripEnd (line j) = j
          | otherwise = search (j + 1)

    divFence k
      | ":::" `T.isPrefixOf` T.stripStart (line k) = Just (search (k + 1))
      | otherwise = Nothing
      where
        search j
          | j >= count = count - 1
          | ":::" `T.isPrefixOf` T.stripStart (line j) = j
          | otherwise = search (j + 1)

    indentedCode k
      | k > 0 && blank (k - 1) && isIndented (line k) = Just (consume k)
      | otherwise = Nothing
      where
        consume j
          | j + 1 >= count = j
          | isIndented (line (j + 1)) = consume (j + 1)
          | blank (j + 1) && j + 2 < count && isIndented (line (j + 2)) = consume (j + 2)
          | otherwise = j

isFence :: T.Text -> Bool
isFence l = "```" `T.isPrefixOf` T.stripStart l || "~~~" `T.isPrefixOf` T.stripStart l

isHeading :: T.Text -> Bool
isHeading l =
  let t = T.stripStart l
      hashes = T.takeWhile (== '#') t
  in not (T.null hashes) && T.length hashes <= 6
     && (T.length hashes == T.length t || T.index t (T.length hashes) == ' ')

isSetextUnderline :: T.Text -> Bool
isSetextUnderline l =
  let t = T.strip l
  in T.length t >= 3 && (T.all (== '=') t || T.all (== '-') t)

isRule :: T.Text -> Bool
isRule l =
  let t = T.filter (/= ' ') (T.strip l)
  in T.length t >= 3 && (T.all (== '-') t || T.all (== '*') t || T.all (== '_') t)

isRefDef :: T.Text -> Bool
isRefDef l =
  let t = T.stripStart l
  in "[" `T.isPrefixOf` t && not ("[^" `T.isPrefixOf` t) && "]" `T.isInfixOf` t
     && "]:" `T.isInfixOf` t

isFootnoteDef :: T.Text -> Bool
isFootnoteDef l = "[^" `T.isPrefixOf` T.stripStart l && "]:" `T.isInfixOf` l

isTableLine :: T.Text -> Bool
isTableLine l = "|" `T.isPrefixOf` T.stripStart l

-- | A pipe table has a delimiter row or more than one cell per line; a lone
-- `| text` line is a Pandoc line block.
isPipeTableLine :: T.Text -> Bool
isPipeTableLine l =
  let t = T.strip l
      cells = T.count "|" t
      delim = not (T.null t) && T.all (`elem` ("|-: " :: String)) t && "-" `T.isInfixOf` t
  in cells >= 2 || delim

-- | The rule line of a Pandoc simple table, e.g. `    -----  ----`.
isSimpleTableDelim :: T.Text -> Bool
isSimpleTableDelim l =
  let t = T.strip l
  in not (T.null t) && T.all (`elem` ("- " :: String)) t && "--" `T.isInfixOf` t

isDashLine :: T.Text -> Bool
isDashLine l =
  let t = T.strip l
  in T.length t >= 3 && T.all (== '-') t

isGridTable :: T.Text -> Bool
isGridTable l =
  let t = T.stripStart l
  in "+" `T.isPrefixOf` t && "-" `T.isInfixOf` t

isRawHtml :: T.Text -> Bool
isRawHtml l =
  let t = T.stripStart l
      name = T.toLower (T.takeWhile isTagChar (T.drop 1 t))
  in "<!--" `T.isPrefixOf` t
     || "<!" `T.isPrefixOf` t
     || "<?" `T.isPrefixOf` t
     || "</" `T.isPrefixOf` t
     || ("<" `T.isPrefixOf` t && name `elem` htmlBlockTags)
  where
    isTagChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | HTML block-level elements. Anything else (span, a, em, code, ...) is inline
-- HTML and belongs to a paragraph, exactly as Pandoc's reader treats it.
htmlBlockTags :: [T.Text]
htmlBlockTags =
  [ "address", "article", "aside", "blockquote", "body", "canvas", "caption"
  , "col", "colgroup", "dd", "details", "dialog", "div", "dl", "dt", "fieldset"
  , "figcaption", "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6"
  , "head", "header", "hr", "html", "iframe", "legend", "li", "main", "menu"
  , "nav", "noscript", "object", "ol", "optgroup", "option", "p", "param", "pre"
  , "script", "section", "select", "source", "style", "summary", "table", "tbody"
  , "td", "textarea", "tfoot", "th", "thead", "title", "tr", "track", "ul", "video"
  ]

isLatexMacro :: T.Text -> Bool
isLatexMacro l = any (`T.isPrefixOf` T.stripStart l)
  (["\\newcommand", "\\renewcommand", "\\providecommand", "\\def", "\\DeclareMathOperator"] :: [T.Text])

isBlockQuote :: T.Text -> Bool
isBlockQuote l = ">" `T.isPrefixOf` T.stripStart l

isIndented :: T.Text -> Bool
isIndented l = "    " `T.isPrefixOf` l || "\t" `T.isPrefixOf` l

isListStart :: T.Text -> Bool
isListStart l =
  let t = T.stripStart l
  in (T.length t >= 2 && T.head t `elem` ("-*+" :: String) && T.index t 1 == ' ')
     || ordered t
     || "(@" `T.isPrefixOf` t
  where
    ordered t =
      let digits = T.takeWhile (`elem` ("0123456789" :: String)) t
          rest = T.drop (T.length digits) t
      in not (T.null digits) && T.length rest >= 2
         && T.head rest `elem` (".)" :: String) && T.index rest 1 == ' '


-- | Public API: describe a project's editable structure without writing.
inspectValue :: FilePath -> IO Value
inspectValue project = do
  outcome <- try (inspectProject project) :: IO (Either PandocError Value)
  pure $ case outcome of
    Left err -> failure "INSPECT_FAILED" (T.pack (show err))
    Right value -> value

inspectProject :: FilePath -> IO Value
inspectProject project = do
  root <- makeAbsolute project
  (_, sources, supplements) <- runIOorExplode $ readYamlWithSources (root </> "metadata.yaml")
  files <- mapM (named root "source") sources
  extras <- mapM (named root "supplement") supplements
  pure $ object
    [ "version" .= (1 :: Int)
    , "project" .= root
    , "sources" .= sources
    , "supplement" .= supplements
    , "files" .= (metadataValue : files <> extras)
    ]
  where
    named root role name = do
      value <- inspectFile (root </> name)
      pure $ withFields value [("path", String (T.pack name)), ("role", String (T.pack role))]
    metadataValue = object
      [ "path" .= ("metadata.yaml" :: T.Text)
      , "role" .= ("metadata" :: T.Text)
      , "mapped" .= False
      , "reason" .= ("metadata editing is not implemented yet" :: T.Text)
      , "blocks" .= ([] :: [Value])
      ]

withFields :: Value -> [(T.Text, Value)] -> Value
withFields (Object o) fields = Object (foldr (\(k, v) acc -> KM.insert (Key.fromText k) v acc) o fields)
withFields value _ = value

-- | Public API: validate one replacement against the current file. Never writes.
planEditValue :: FilePath -> BL.ByteString -> IO Value
planEditValue project input =
  case decodeInput input of
    Left problem -> pure (failure "INVALID_ARGUMENT" problem)
    Right (path, start, end, replacement) -> do
      outcome <- try (planProject project path start end replacement) :: IO (Either PandocError Value)
      pure $ case outcome of
        Left err -> failure "EDIT_FAILED" (T.pack (show err))
        Right value -> value

planProject :: FilePath -> FilePath -> Int -> Int -> T.Text -> IO Value
planProject project path start end replacement = do
  root <- makeAbsolute project
  (_, sources, supplements) <- runIOorExplode $ readYamlWithSources (root </> "metadata.yaml")
  if path `notElem` (sources <> supplements)
    then pure (failure "INVALID_PATH" "the edit must target a listed source or supplement file")
    else do
      raw <- BS.readFile (root </> path)
      case TE.decodeUtf8' raw of
        Left _ -> pure (failure "ENCODING" "the file is not valid UTF-8")
        Right text ->
          case splice text start end replacement of
            Left problem -> pure (failure "INVALID_BLOCK" problem)
            Right (next, index, kind) -> do
              oldBlocks <- unBlocks <$> runIOorExplode (readText' text)
              newBlocks <- unBlocks <$> runIOorExplode (readText' (TE.decodeUtf8 next))
              pure $ case validate index kind oldBlocks newBlocks of
                Just problem -> failure "STRUCTURE_CHANGED" problem
                Nothing -> object
                  [ "ok" .= True
                  , "path" .= path
                  , "text" .= TE.decodeUtf8 next
                  ]

decodeInput :: BL.ByteString -> Either T.Text (FilePath, Int, Int, T.Text)
decodeInput body =
  case eitherDecode body of
    Left problem -> Left (T.pack problem)
    Right value -> case value of
      Object o -> do
        path <- textField o "path"
        start <- intField o "byteStart"
        end <- intField o "byteEnd"
        bodyText <- textField o "text"
        if end < start
          then Left "byteEnd is before byteStart"
          else Right (T.unpack path, start, end, bodyText)
      _ -> Left "expected a JSON object"
  where
    textField o key = case KM.lookup (Key.fromText key) o of
      Just (String value) -> Right value
      _ -> Left ("missing string field " <> key)
    intField o key = case KM.lookup (Key.fromText key) o of
      Just (Number value) -> Right (floor value)
      _ -> Left ("missing integer field " <> key)

-- | Replace one byte range that exactly matches a scanned block.
splice :: T.Text -> Int -> Int -> T.Text -> Either T.Text (BS.ByteString, Int, T.Text)
splice text start end replacement = do
  let raw = TE.encodeUtf8 text
      total = BS.length raw
      segments = scanLines text
      hits = [ (i, seg) | (i, seg) <- zip [0 :: Int ..] segments
                        , segStart seg == start, segEnd seg == end ]
  if start < 0 || end > total
    then Left "the block range is outside the file"
    else case hits of
      [] -> Left "the range does not match a current block; refresh before editing"
      ((i, seg) : _) ->
        let headBytes = BS.take start raw
            tailBytes = BS.drop end raw
        in case TE.decodeUtf8' headBytes of
          Left _ -> Left "the block range is not on a character boundary"
          Right _ ->
            let slice = sliceText text seg
                trailing = if "\r\n" `T.isSuffixOf` slice then "\r\n"
                           else if "\n" `T.isSuffixOf` slice then "\n" else ""
                middle = TE.encodeUtf8 (replacement <> trailing)
            in Right (headBytes <> middle <> tailBytes, i, segKind seg)

validate :: Int -> T.Text -> [Block] -> [Block] -> Maybe T.Text
validate index kind oldBlocks newBlocks
  | length oldBlocks /= length newBlocks =
      Just "the replacement changed the number of blocks; edit one block at a time"
  | index >= length newBlocks = Just "the edited block no longer exists"
  | take index newBlocks /= take index oldBlocks =
      Just "the replacement would change a block before the edited one"
  | drop (index + 1) newBlocks /= drop (index + 1) oldBlocks =
      Just "the replacement would change a block after the edited one"
  | blockKindName (newBlocks !! index) /= kind =
      Just ("the replacement reads as " <> blockKindName (newBlocks !! index) <> ", not " <> kind)
  | otherwise = Nothing

failure :: T.Text -> T.Text -> Value
failure code message = object
  [ "ok" .= False
  , "error" .= object ["code" .= code, "message" .= message]
  ]
