{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lambda.UI.Markdown
  ( renderMarkdown
  , parseMarkdownBlocks
  , tokenizeInlines
  , greedyWrap
  , budgetWidths
  , initVtyUnicodeWidthTable
  , textVisualWidth
  , tokenVisualWidth
  , MdBlock(..)
  , WordToken(..)
  ) where

import Brick
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Table
  ( table
  , renderTable
  , surroundingBorder
  , rowBorders
  , columnBorders
  )
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Vty (safeWcwidth)
import qualified Graphics.Vty.UnicodeWidthTable.Types as VtyTable
import qualified Graphics.Vty.UnicodeWidthTable.Install as VtyInstall

import Lambda.UI.Types (ResourceName)

-- | Atomic styled word token for inline layout and word wrapping
data WordToken = WordToken
  { tokAttr  :: !AttrName
  , tokText  :: !Text
  , tokSpace :: !Bool
  } deriving (Show, Eq)

-- | Abstract representation of scanned Markdown blocks
data MdBlock
  = MdH1 ![WordToken]
  | MdH2 ![WordToken]
  | MdH3 ![WordToken]
  | MdCode !(Maybe Text) ![Text]
  | MdTable ![[Text]]
  | MdBullet ![WordToken]
  | MdNumbered !Int ![WordToken]
  | MdQuote ![WordToken]
  | MdThematicBreak
  | MdPara ![WordToken]
  deriving (Show, Eq)

-- | Render Markdown text into a composite Brick Widget
renderMarkdown :: Text -> Widget ResourceName
renderMarkdown = vBox . map renderMdBlock . parseMarkdownBlocks

-- | Streaming-resilient line-by-line block parser
parseMarkdownBlocks :: Text -> [MdBlock]
parseMarkdownBlocks rawText = go (T.lines rawText)
  where
    go [] = []
    go (l:ls)
      -- Skip empty blank lines between blocks
      | T.null (T.strip l) = go ls

      -- Fenced code blocks (naturally resilient to unclosed fences during active streaming)
      | "```" `T.isPrefixOf` l =
          let lang = let rest = T.strip (T.drop 3 l) in if T.null rest then Nothing else Just rest
              (codeLines, remainder) = break ("```" `T.isPrefixOf`) ls
              restLs = case remainder of
                []          -> []
                (_:restRem) -> restRem
          in MdCode lang codeLines : go restLs

      -- Headings
      | "# " `T.isPrefixOf` l   = MdH1 (tokenizeInlines (T.drop 2 l)) : go ls
      | "## " `T.isPrefixOf` l  = MdH2 (tokenizeInlines (T.drop 3 l)) : go ls
      | "### " `T.isPrefixOf` l = MdH3 (tokenizeInlines (T.drop 4 l)) : go ls

      -- Thematic breaks / Horizontal rules
      | T.strip l `elem` ["---", "***", "___"] = MdThematicBreak : go ls

      -- GFM Pipe Tables
      | isTableLine l =
          let (tableLines, remainder) = span isTableLine (l:ls)
              dataRows = map parseTableRow (filter (not . isSeparatorRow) tableLines)
          in if null dataRows
               then go remainder
               else MdTable (normalizeTable dataRows) : go remainder

      -- Blockquotes (consecutive > lines grouped)
      | isQuoteLine l =
          let (quoteLines, remainder) = span isQuoteLine (l:ls)
              cleaned = T.unwords (map cleanQuote quoteLines)
          in MdQuote (tokenizeInlines cleaned) : go remainder

      -- Bullet lists
      | "- " `T.isPrefixOf` l = MdBullet (tokenizeInlines (T.drop 2 l)) : go ls
      | "* " `T.isPrefixOf` l = MdBullet (tokenizeInlines (T.drop 2 l)) : go ls

      -- Numbered lists
      | isNumberedList l =
          let (num, rest) = parseNumbered l
          in MdNumbered num (tokenizeInlines rest) : go ls

      -- Paragraphs (group contiguous prose lines)
      | otherwise =
          let (paraLines, remainder) = span isParaLine (l:ls)
              paraText = T.unwords (map T.strip paraLines)
          in MdPara (tokenizeInlines paraText) : go remainder

    isTableLine line =
      let s = T.strip line
      in "|" `T.isPrefixOf` s && "|" `T.isSuffixOf` s && T.count "|" s >= 2

    isSeparatorRow line =
      let s = T.filter (`notElem` [' ', '|', ':', '-']) line
      in T.null s && "-" `T.isInfixOf` line

    parseTableRow line =
      let parts = T.splitOn "|" (T.strip line)
          trimmed = case parts of
            ("":rest) -> case reverse rest of
                           ("":rrest) -> reverse rrest
                           _          -> rest
            _         -> parts
      in map T.strip trimmed

    normalizeTable [] = []
    normalizeTable rows =
      let maxCols = maximum (map length rows)
          padRow r = r ++ replicate (maxCols - length r) ""
      in map padRow rows

    isQuoteLine line =
      let s = T.strip line
      in ">" `T.isPrefixOf` s

    cleanQuote line =
      let s = T.stripStart line
      in if "> " `T.isPrefixOf` s
           then T.drop 2 s
           else if ">" `T.isPrefixOf` s
                  then T.drop 1 s
                  else s

    isNumberedList line =
      let stripped = T.stripStart line
          (digits, rest) = T.span (`elem` ['0'..'9']) stripped
      in not (T.null digits) && (". " `T.isPrefixOf` rest)

    parseNumbered line =
      let stripped = T.stripStart line
          (digits, rest) = T.span (`elem` ['0'..'9']) stripped
          n = case reads (T.unpack digits) of
                [(val, "")] -> val
                _           -> 1
          content = T.drop 2 rest
      in (n, content)

    isParaLine line =
      let s = T.strip line
      in not (T.null s)
           && not ("```" `T.isPrefixOf` line)
           && not ("# " `T.isPrefixOf` line)
           && not ("## " `T.isPrefixOf` line)
           && not ("### " `T.isPrefixOf` line)
           && not (s `elem` ["---", "***", "___"])
           && not (isTableLine line)
           && not (isQuoteLine line)
           && not ("- " `T.isPrefixOf` line)
           && not ("* " `T.isPrefixOf` line)
           && not (isNumberedList line)

-- | Tokenize mixed inline markdown (bold, italic, code spans, plain text)
-- Correctly tracks whitespace across span boundaries to prevent word gluing.
tokenizeInlines :: Text -> [WordToken]
tokenizeInlines rawInput = convertSpans (parseSpans rawInput)
  where
    parseSpans t
      | T.null t = []
      | "**" `T.isPrefixOf` t =
          let (inside, after) = T.breakOn "**" (T.drop 2 t)
          in if "**" `T.isPrefixOf` after
               then (attrName "mdBold", inside) : parseSpans (T.drop 2 after)
               else (attrName "mdNormal", "**") : parseSpans (T.drop 2 t)
      | "`" `T.isPrefixOf` t =
          let (inside, after) = T.breakOn "`" (T.drop 1 t)
          in if "`" `T.isPrefixOf` after
               then (attrName "mdCodeInline", inside) : parseSpans (T.drop 1 after)
               else (attrName "mdNormal", "`") : parseSpans (T.drop 1 t)
      | "*" `T.isPrefixOf` t =
          let (inside, after) = T.breakOn "*" (T.drop 1 t)
          in if "*" `T.isPrefixOf` after && not (T.null inside)
               then (attrName "mdItalic", inside) : parseSpans (T.drop 1 after)
               else (attrName "mdNormal", "*") : parseSpans (T.drop 1 t)
      | otherwise =
          let (plain, after) = T.break (`elem` ['*', '`']) t
          in if T.null plain
               then (attrName "mdNormal", T.singleton (T.head t)) : parseSpans (T.tail t)
               else (attrName "mdNormal", plain) : parseSpans after

    convertSpans [] = []
    convertSpans ((attr, strVal) : nextSpans) =
      let ws = T.words strVal
          hasOwnTrail = not (T.null strVal) && isSpace (T.last strVal)
          nextHasLead = case nextSpans of
            ((_, nxtStr) : _) -> not (T.null nxtStr) && isSpace (T.head nxtStr)
            []                -> False
          trail = hasOwnTrail || nextHasLead
      in case ws of
           []  -> convertSpans nextSpans
           [w] -> WordToken attr w trail : convertSpans nextSpans
           _   -> [ WordToken attr w True | w <- init ws ]
                  ++ [WordToken attr (last ws) trail]
                  ++ convertSpans nextSpans

-- | Configure Vty's global Unicode width table so that emojis and keycap sequences
-- occupy their true visual column width (2 terminal cells), preventing jagged borders in tables.
initVtyUnicodeWidthTable :: IO ()
initVtyUnicodeWidthTable = do
  let rKeycap = VtyTable.WidthTableRange 0x20E3 1 1     -- Combining enclosing keycap (turns 1️⃣..9️⃣ into width 2)
      rTime1  = VtyTable.WidthTableRange 0x231A 2 2     -- ⌚, ⌛
      rTime2  = VtyTable.WidthTableRange 0x23E9 12 2    -- ⏩..⏳ (0x23E9..0x23F3)
      rDing   = VtyTable.WidthTableRange 0x2600 0x200 2 -- Misc Symbols & Dingbats (0x2600..0x27BF, e.g. ⚠️, ✅, ❌, ✨)
      rStar   = VtyTable.WidthTableRange 0x2B50 6 2     -- ⭐, ⭕, etc.
      rEmoji1 = VtyTable.WidthTableRange 0x1F000 0xB00 2 -- SMP Emojis & Pictographs (0x1F000..0x1FAFF)
      tbl = VtyTable.UnicodeWidthTable [rKeycap, rTime1, rTime2, rDing, rStar, rEmoji1]
  VtyInstall.installUnicodeWidthTable tbl

-- | Compute visual terminal column width of a text string using Vty's active width table
textVisualWidth :: Text -> Int
textVisualWidth = T.foldl' (\acc c -> acc + max 0 (safeWcwidth c)) 0

-- | Compute visual terminal column width of a single word token
tokenVisualWidth :: WordToken -> Int
tokenVisualWidth t = textVisualWidth (tokText t) + (if tokSpace t then 1 else 0)

-- | Pure greedy line-breaking algorithm based on target column width
greedyWrap :: Int -> [WordToken] -> [[WordToken]]
greedyWrap maxW tokens
  | maxW <= 0 = [tokens]
  | otherwise = go 0 [] tokens
  where
    go _ currentLine [] =
      if null currentLine then [] else [reverse currentLine]
    go curW currentLine (t:ts) =
      let w = tokenVisualWidth t
      in if curW + w <= maxW || null currentLine
           then go (curW + w) (t : currentLine) ts
           else reverse currentLine : go 0 [] (t : ts)

-- | Render wrapped styled inline tokens responsive to dynamic terminal width
renderWrappedInlines :: [WordToken] -> Widget ResourceName
renderWrappedInlines [] = emptyWidget
renderWrappedInlines tokens = Widget Greedy Fixed $ do
  ctx <- getContext
  let maxW = availWidth ctx
      lines' = greedyWrap maxW tokens
  render $ vBox [ hBox [ withAttr (tokAttr t) (txt (tokText t <> if tokSpace t then " " else "")) | t <- line ] | line <- lines' ]

-- | Render individual Markdown block elements to Brick Widgets
renderMdBlock :: MdBlock -> Widget ResourceName
renderMdBlock (MdH1 tokens) =
  padTop (Pad 1) $ padBottom (Pad 1) $
    hBox [withAttr (attrName "mdH1") (str "# "), renderWrappedInlines tokens]

renderMdBlock (MdH2 tokens) =
  padTop (Pad 1) $
    hBox [withAttr (attrName "mdH2") (str "## "), renderWrappedInlines tokens]

renderMdBlock (MdH3 tokens) =
  hBox [withAttr (attrName "mdH3") (str "### "), renderWrappedInlines tokens]

renderMdBlock (MdCode mLang codeLines) =
  padBottom (Pad 1) $
    vBox
      [ withAttr (attrName "mdCodeLang") (str $ "● " <> maybe "code" T.unpack mLang)
      , padLeft (Pad 2) $
          withAttr (attrName "mdCodeBlock") $
            vBox (map (txt . padEmpty) codeLines)
      ]
  where
    padEmpty l = if T.null l then " " else l

renderMdBlock (MdTable rows) =
  padTopBottom 1 $ renderMdTable rows

renderMdBlock (MdBullet tokens) =
  hBox
    [ withAttr (attrName "mdBullet") (str "• ")
    , renderWrappedInlines tokens
    ]

renderMdBlock (MdNumbered n tokens) =
  hBox
    [ withAttr (attrName "mdNumbered") (str (show n <> ". "))
    , renderWrappedInlines tokens
    ]

renderMdBlock (MdQuote tokens) =
  padBottom (Pad 1) $
    hBox
      [ withAttr (attrName "mdQuoteBar") (str "▎ ")
      , renderWrappedInlines tokens
      ]

renderMdBlock MdThematicBreak =
  padTopBottom 1 $ withAttr (attrName "mdRule") hBorder

renderMdBlock (MdPara tokens) =
  padBottom (Pad 1) $ renderWrappedInlines tokens

-- | Render GFM Table using Brick.Widgets.Table with dynamic column budgeting
renderMdTable :: [[Text]] -> Widget ResourceName
renderMdTable [] = emptyWidget
renderMdTable allRows@(headerRow : dataRows) = Widget Greedy Fixed $ do
  ctx <- getContext
  let totalAvail = availWidth ctx
      numCols = length headerRow
      -- Frame overhead: (numCols + 1) vertical borders + (2 * numCols) cell padding spaces
      frameOverhead = (numCols + 1) + (2 * numCols)
      usableWidth = max (numCols * 12) (totalAvail - frameOverhead)

      -- Measure natural width of each column (longest cell visual width in columns)
      naturalWidths =
        [ maximum (0 : [ textVisualWidth (row !! colIdx) | row <- allRows, colIdx < length row ])
        | colIdx <- [0 .. numCols - 1]
        ]
      totalNatural = sum naturalWidths

      -- Budget column widths proportionally, bounded by a reasonable minimum
      minColW = max 12 (usableWidth `div` (numCols * 2))
      colWidths =
        if totalNatural <= usableWidth
          then naturalWidths
          else budgetWidths usableWidth minColW naturalWidths

      fmtHeader colIdx c =
        let w = if colIdx < length colWidths then colWidths !! colIdx else 20
        in withAttr (attrName "mdTableHeader") (renderWrappedCell w c)

      fmtData colIdx c =
        let w = if colIdx < length colWidths then colWidths !! colIdx else 20
        in withAttr (attrName "mdTableCell") (renderWrappedCell w c)

      formattedRows =
        zipWith fmtHeader [0..] headerRow :
        map (\row -> zipWith fmtData [0..] row) dataRows

      tbl = surroundingBorder True $
            rowBorders True $
            columnBorders True $
            table formattedRows
  render (renderTable tbl)

-- | Proportionally allocate available width to columns with a minimum width bound
budgetWidths :: Int -> Int -> [Int] -> [Int]
budgetWidths totalUsable minW nats =
  let sumNats = max 1 (sum nats)
      rawBudgets = map (\n -> max minW ((totalUsable * n) `div` sumNats)) nats
      diff = totalUsable - sum rawBudgets
  in case rawBudgets of
       []     -> []
       (b:bs) -> (b + diff) : bs

-- | Visual display width of a line of tokens in terminal columns
lineVisualWidth :: [WordToken] -> Int
lineVisualWidth = sum . map tokenVisualWidth

-- | Render wrapped styled inlines inside a table cell bounded by target column width
renderWrappedCell :: Int -> Text -> Widget ResourceName
renderWrappedCell targetW cellText =
  let tokens = tokenizeInlines cellText
      wrappedLines = greedyWrap targetW tokens
      renderLine line =
        let lw = lineVisualWidth line
            padLen = max 0 (targetW - lw)
            padWidget = str (replicate padLen ' ')
            lineTokens = [ withAttr (tokAttr tok) (txt (tokText tok <> if tokSpace tok then " " else ""))
                         | tok <- line ]
        in hBox (lineTokens ++ [padWidget])
  in case wrappedLines of
       [] -> str (replicate (max 1 targetW) ' ')
       _  -> vBox [ renderLine line | line <- wrappedLines ]
