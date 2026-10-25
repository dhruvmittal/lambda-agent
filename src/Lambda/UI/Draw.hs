{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Draw
  ( drawApp
  , renderSubAgents
  , renderSubAgentsSelected
  , renderInlineSubAgent
  , renderCompletionLine
  , renderPowerlinePrompt
  , renderPowerlinePromptWithElapsed
  , headerBar
  , headerBarWithElapsed
  , formatElapsed
  , wrapLineWithSpans
  , wrapLineWithWidths
  , wrapEditorToVisual
  , wrapEditorToVisualWithFirstWidth
  , distributeSubAgentsToTurns
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Lens.Micro ((^.), to)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse, partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Time.Format (defaultTimeLocale, formatTime)

import Lambda.Config (Config(..), extractProviderAndModel)
import Lambda.Engine.Compactor (estimateTotalTokens)
import Lambda.Engine.Session (SessionMeta(..))
import Lambda.Types
import Lambda.UI.Completion (slidingCandidateWindow)
import Lambda.UI.Markdown (renderMarkdown, textVisualWidth)
import Lambda.UI.Types

drawApp :: UIState -> [Widget ResourceName]
drawApp st = [modalOverlay st, sessionChooserOverlay st, hudOverlay st, subAgentDrawerOverlay st, mainLayout st, fill ' ']

sessionChooserOverlay :: UIState -> Widget ResourceName
sessionChooserOverlay UIState{ uiSessionChooser = Just SessionChooserState{..} } =
  C.centerLayer $
    withBorderStyle BS.unicodeRounded $
      B.borderWithLabel (withAttr (attrName "hudTitle") (str " Select Session [Esc to close] ")) $
        hLimit 74 $ vLimit 18 $
          padAll 1 $
            vBox
              [ if null scSessions
                  then C.hCenter (padAll 2 $ withAttr (attrName "thinkingDim") (str "No saved sessions found."))
                  else vBox (zipWith renderRow [0..] visibleRows)
              , vLimit 1 (fill ' ')
              , withBorderStyle BS.unicodeRounded B.hBorder
              , padTop (Pad 0) $ C.hCenter $
                  withAttr (attrName "thinkingDim") $
                    str "[1-9] Quick Switch  •  ↑/↓: Navigate  •  Enter: Select  •  Esc: Cancel"
              ]
  where
    maxVisible = 9
    total = length scSessions
    half = maxVisible `div` 2
    startIdx = max 0 (min (scSelected - half) (total - maxVisible))
    visibleRows = take maxVisible (drop startIdx (zip [1 :: Int ..] scSessions))

    renderRow listIdx (num, m) =
      let itemGlobalIdx = startIdx + listIdx
          isSel = itemGlobalIdx == scSelected
          isActive = metaId m == scActiveId
          cursorStr = if isSel then " ❯ " else "   "
          numStr = "[" <> show num <> "] "
          timeStr = formatTime defaultTimeLocale "%H:%M" (metaUpdatedAt m)
          turnsStr = show (metaTurnCount m) <> "t"
          forkStr = case metaParentId m of
            Just _  -> " ↳frk"
            Nothing -> ""
          activeStr = if isActive then " ★" else ""
          rawTitle = if T.null (metaTitle m) then "Untitled Session" else metaTitle m
          shortTitle = if T.length rawTitle > 34 then T.take 32 rawTitle <> "…" else rawTitle

          cursorWidget = if isSel
                           then withAttr (attrName "compSelected") (str cursorStr)
                           else withAttr (attrName "thinkingDim") (str cursorStr)
          numWidget = withAttr (attrName "hudKey") (str numStr)
          timeWidget = withAttr (attrName "thinkingDim") (str (timeStr <> " · "))
          titleWidget = if isSel
                          then withAttr (attrName "compSelected") (txt shortTitle)
                          else withAttr (attrName "mdNormal") (txt shortTitle)
          metaWidget = withAttr (attrName "thinkingDim") (str (" " <> turnsStr <> forkStr <> activeStr))

          rowContent = clickable (SessionItem (num - 1)) $ hBox
            [ cursorWidget
            , numWidget
            , timeWidget
            , titleWidget
            , fill ' '
            , metaWidget
            ]
      in if isSel
           then withAttr (attrName "compSelected") rowContent
           else rowContent
sessionChooserOverlay _ = emptyWidget

modalOverlay :: UIState -> Widget ResourceName
modalOverlay UIState{ uiCurrentPrompt = Just p } =
  C.centerLayer $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str " Security Gate: Authorization Required ") $
        hLimit 72 $ vLimit 14 $
          padAll 1 $
            vBox
              [ padBottom (Pad 1) $ str "An operation requires user confirmation:"
              , str $ "Caller:  " <> show (promptCaller p)
              , str $ "Tool:    " <> T.unpack (promptTool p)
              , padBottom (Pad 1) $ txtWrap ("Args:    " <> T.pack (take 60 (show (promptArgs p))))
              , if not (T.null (promptProposedGlob p))
                  then padBottom (Pad 1) $ hBox [withAttr (attrName "hudKey") (str "Pattern: "), str (T.unpack (promptProposedGlob p))]
                  else emptyWidget
              , C.hCenter $
                  hBox
                    [ clickable ButtonAlways  $ B.border (str " [1] Always (Config) ")
                    , str " "
                    , clickable ButtonSession $ B.border (str " [2] Session ")
                    , str " "
                    , clickable ButtonOnce    $ B.border (str " [3] Once ")
                    , str " "
                    , clickable ButtonDeny    $ B.border (str " [4] Deny ")
                    ]
              ]
modalOverlay _ = emptyWidget

hudOverlay :: UIState -> Widget ResourceName
hudOverlay UIState{ uiShowHud = True, .. } =
  C.centerLayer $
    withBorderStyle BS.unicodeRounded $
      B.borderWithLabel (withAttr (attrName "hudTitle") (str " lambdA Intelligence HUD [Alt+H / F1 or Esc to close] ")) $
        hLimit 76 $ vLimit 24 $
          padAll 1 $
            vBox
              [ withAttr (attrName "hudSection") (str "◆ ACTIVE GOAL")
              , padLeft (Pad 2) $ txtWrap (extractGoal uiWorkingState)
              , padTop (Pad 1) $ withAttr (attrName "hudSection") (str "◆ INVARIANTS")
              , padLeft (Pad 2) $ renderInvariants uiWorkingState
              , padTop (Pad 1) $ withAttr (attrName "hudSection") (str "◆ ACTIVE HYPOTHESIS")
              , padLeft (Pad 2) $ txtWrap (extractHypothesis uiWorkingState)
              , padTop (Pad 1) $ withAttr (attrName "hudSection") (str "◆ BLOCKED ON")
              , padLeft (Pad 2) $ txtWrap (extractBlocked uiWorkingState)
              , padTop (Pad 1) $ withAttr (attrName "hudSection") (str ("◆ SUBAGENTS (" <> show (Map.size uiSubAgents) <> ")"))
              , padLeft (Pad 2) $ renderSubAgentList uiSubAgents
              , padTop (Pad 1) $ withAttr (attrName "hudSection") (str "◆ KEYBINDINGS")
              , padLeft (Pad 2) $
                  vBox
                    [ hBox [ withAttr (attrName "hudKey") (str "Tab            "), str "Contextual autocomplete (commands, sessions, paths)" ]
                    , hBox [ withAttr (attrName "hudKey") (str "Alt+M / F2     "), str "Toggle Plan / Exec mode" ]
                    , hBox [ withAttr (attrName "hudKey") (str "Alt+, / Alt+.  "), str "Navigate SubAgents (< and >) / Esc to return" ]
                    , hBox [ withAttr (attrName "hudKey") (str "^P / ^N (Alt) "), str "Previous / Next prompt history" ]
                    , hBox [ withAttr (attrName "hudKey") (str "Alt+H / F1     "), str "Toggle this Intelligence HUD" ]
                    , hBox [ withAttr (attrName "hudKey") (str "^T             "), str "Toggle Thinking/Reasoning visibility" ]
                    , hBox [ withAttr (attrName "hudKey") (str "Esc Esc        "), str "Interrupt active generation or tool dispatch" ]
                    , hBox [ withAttr (attrName "hudKey") (str "/help          "), str "Show full command reference in chat" ]
                    ]
              ]
  where
    extractGoal st =
      case filter ("GOAL:" `T.isPrefixOf`) (T.lines st) of
        (g:_) -> T.strip (T.drop 5 g)
        []    -> "Awaiting task"
    extractHypothesis st =
      case filter ("ACTIVE_HYPOTHESIS:" `T.isPrefixOf`) (T.lines st) of
        (h:_) -> T.strip (T.drop 18 h)
        []    -> "None"
    extractBlocked st =
      case filter ("BLOCKED_ON:" `T.isPrefixOf`) (T.lines st) of
        (b:_) -> T.strip (T.drop 11 b)
        []    -> "None (Ready)"
    renderInvariants st =
      case filter ("INVARIANTS:" `T.isPrefixOf`) (T.lines st) of
        (inv:_) -> txtWrap (T.strip (T.drop 11 inv))
        []      -> str "No active invariants specified."
    renderSubAgentList subs
      | Map.null subs = withAttr (attrName "thinkingDim") $ str "No active subagents"
      | otherwise = vBox (map renderSubShort (Map.elems subs))
    renderSubShort SubAgentTask{..} =
      hBox
        [ withAttr (attrName "subRole") (str $ "[" <> T.unpack (T.toUpper subAgentRole) <> "] #" <> show subAgentId)
        , str " - "
        , withAttr (statusAttr subAgentStatus) (str $ formatStatus subAgentStatus)
        , str " "
        , withAttr (attrName "thinkingDim") (str $ "(" <> show subAgentTurnCount <> "/" <> show subAgentBudget <> "t)")
        ]
hudOverlay _ = emptyWidget

rightSideLayer :: Widget n -> Widget n
rightSideLayer p =
  Widget Greedy (vSize p) $ do
    result <- render p
    c <- getContext
    let rWidth = result ^. imageL . to V.imageWidth
        leftPaddingAmount = max 0 (c ^. availWidthL - rWidth)
        off = Location (leftPaddingAmount, 0)
    render $ translateBy off $ Widget Fixed Fixed $ return result

subAgentDrawerOverlay :: UIState -> Widget ResourceName
subAgentDrawerOverlay UIState{ uiSelectedSubAgent = Just sId, .. } =
  case Map.lookup sId uiSubAgents of
    Nothing   -> emptyWidget
    Just task ->
      rightSideLayer $
        hLimitPercent 46 $
          vLimitPercent 100 $
            withBorderStyle BS.unicodeRounded $
              B.borderWithLabel (withAttr (attrName "selectedBadge") (str $ " SubAgent #" <> show sId <> " [" <> T.unpack (T.toUpper (subAgentRole task)) <> "] ")) $
                vBox
                  [ viewport SubAgentView Vertical $
                      padLeftRight 1 $
                        if null (subAgentTurns task)
                          then padAll 1 (withAttr (attrName "thinkingDim") (str "Awaiting dialogue turns recorded for this subagent..."))
                          else vBox (map renderTurn (subAgentTurns task))
                  , vLimit 1 $ withBorderStyle BS.unicodeRounded B.hBorder
                  , vLimit 1 $ padLeftRight 1 $
                      hBox
                        [ withAttr (statusAttr (subAgentStatus task)) (str $ formatStatus (subAgentStatus task))
                        , str " "
                        , withAttr (attrName "thinkingDim") (str $ "(" <> show (subAgentTurnCount task) <> "/" <> show (subAgentBudget task) <> "t)")
                        , vLimit 1 (fill ' ')
                        , withAttr (attrName "thinkingDim") (str "[Esc] Close  Alt+,/. Next")
                        ]
                  ]
subAgentDrawerOverlay _ = emptyWidget

mainLayout :: UIState -> Widget ResourceName
mainLayout UIState{..} =
  let hasRunningSubs = any (\t -> subAgentStatus t == SubAgentRunning) (Map.elems uiSubAgents)
      isWaiting = uiIsGenerating || hasRunningSubs
      mElapsed = if isWaiting && uiElapsedSeconds > 0
                   then Just (formatElapsed uiElapsedSeconds)
                   else Nothing
  in vBox
    [ headerBarWithElapsed uiWorkingState uiSelectedSubAgent mElapsed
    , renderMainPane uiSelectedSubAgent uiTurns uiSubAgents uiCompletion uiMode uiModelName uiContextLimit uiConfig uiEditor isWaiting uiSpinnerStep mElapsed
    ]

renderMainPane
  :: Maybe Int
  -> [Turn]
  -> Map.Map Int SubAgentTask
  -> Maybe CompletionState
  -> AgentMode
  -> Text
  -> Int
  -> Config
  -> E.Editor Text ResourceName
  -> Bool
  -> Int
  -> Maybe Text
  -> Widget ResourceName
renderMainPane mSelected turns subs uiComp mode model ctxLimit cfg editor isWaiting spinnerStep mElapsed =
  vBox
    [ viewport ChatView Vertical $
        padLeftRight 1 $
          if null turns
            then if null subList
                   then emptyWidget
                   else renderFleetStrip mSelected subList
            else vBox (map renderTurnPair turnPairs)
    , renderCompletionLine uiComp
    , renderPowerlinePromptWithElapsed mode model (apiBaseUrl cfg) turns ctxLimit editor isWaiting spinnerStep mElapsed
    ]
  where
    subList = Map.elems subs
    turnPairs = distributeSubAgentsToTurns turns subList
    renderTurnPair (t, turnSubs) = renderTurnWithSubs mSelected turnSubs t

-- | Partitions subagent tasks across turns according to parent turn attribution
distributeSubAgentsToTurns :: [Turn] -> [SubAgentTask] -> [(Turn, [SubAgentTask])]
distributeSubAgentsToTurns [] _ = []
distributeSubAgentsToTurns turns subList =
  let turnIds = map turnId turns
      knownTurnSet = Set.fromList turnIds
      (attributed, unanchored) = partition (\s -> maybe False (`Set.member` knownTurnSet) (subAgentParentTurnId s)) subList
      subsByTurn = Map.fromListWith (flip (++)) [ (pId, [s]) | s <- attributed, Just pId <- [subAgentParentTurnId s] ]
      attachSubs t = (t, Map.findWithDefault [] (turnId t) subsByTurn)
      pairs = map attachSubs turns
  in case reverse pairs of
       [] -> []
       ((lastT, lastSubs) : older) ->
         reverse ((lastT, lastSubs ++ unanchored) : older)

headerBar :: Text -> Maybe Int -> Widget ResourceName
headerBar workingStateText mSelected = headerBarWithElapsed workingStateText mSelected Nothing

headerBarWithElapsed :: Text -> Maybe Int -> Maybe Text -> Widget ResourceName
headerBarWithElapsed workingStateText mSelected mElapsed =
  vLimit 1 $
    withAttr (attrName "headerBar") $
      hBox
        [ withAttr (attrName "titleLogo") (str " λ lambdA ")
        , str " │ "
        , str "Goal: "
        , hLimit 55 $ txt (wsGoal ws)
        , case mSelected of
            Just sId -> hBox [ str " │ ", withAttr (attrName "selectedBadge") (str $ " [SUBAGENT #" <> show sId <> "] ") ]
            Nothing  -> emptyWidget
        , fill ' '
        , case mElapsed of
            Just el -> hBox [ withAttr (attrName "selectedBadge") (txt $ " [" <> el <> " elapsed · Esc to cancel] "), str " " ]
            Nothing -> emptyWidget
        , txt (case wsBlockedOn ws of
                 Just b  -> "● BLOCKED_ON: " <> b
                 Nothing -> "✓ Ready")
        , str " "
        ]
  where
    ws = parseWorkingState workingStateText

renderTurn :: Turn -> Widget ResourceName
renderTurn = renderTurnWithSubs Nothing []

renderTurnWithSubs :: Maybe Int -> [SubAgentTask] -> Turn -> Widget ResourceName
renderTurnWithSubs _ _ (Turn _ UserRole blocks) =
  padBottom (Pad 1) $
    hBox
      [ withAttr (attrName "userAccent") (str "▎ ")
      , vBox (map renderBlock blocks)
      ]
renderTurnWithSubs mSelected turnSubs (Turn tId role blocks) =
  let hasToolCalls = any isToolCall blocks
      padAmt = if role == AssistantRole && hasToolCalls then 0 else 1
  in padBottom (Pad padAmt) $
    vBox
      [ if role == SystemRole
          then withAttr (attrName "systemRole") (str $ "● system (Turn #" <> show tId <> ")")
          else emptyWidget
      , vBox (map renderBlock blocks)
      , if null turnSubs
          then emptyWidget
          else renderFleetStrip mSelected turnSubs
      , if role == AssistantRole && not hasToolCalls
          then padLeft (Pad 2) $ withAttr (attrName "turnFooter") (str "■ Plan · assistant")
          else emptyWidget
      ]

isToolCall :: ContentBlock -> Bool
isToolCall (ToolCallBlock _) = True
isToolCall _                 = False

renderBlock :: ContentBlock -> Widget ResourceName
renderBlock (TextBlock t) =
  padLeft (Pad 2) $ renderMarkdown t
renderBlock (ThinkingBlock tId body vis) =
  clickable (ThinkingFold tId) $
    padLeft (Pad 2) $
      case vis of
        Collapsed -> withAttr (attrName "thinkingDim") $ str "▶ Thought (press ^T or click to expand)"
        Visible   ->
          hBox
            [ withAttr (attrName "thinkingDim") (str "┊ ")
            , withAttr (attrName "thinkingDim") $
                txtWrap ("Thinking: " <> body)
            ]
renderBlock (ToolCallBlock tc) =
  padLeft (Pad 2) $
    withAttr (attrName "toolCall") $
      let argsStr = formatToolCallArgs tc
          header = "╭─ " <> toolCallName tc <> (if T.null argsStr then "" else " " <> argsStr)
      in txtWrap header
renderBlock (ToolResultBlock tr) =
  padLeft (Pad 2) $
    vBox
      [ if not (T.null (resultStdout tr))
          then renderToolResultStdout (resultStdout tr)
          else emptyWidget
      , if not (T.null (resultStderr tr))
          then withAttr (attrName "toolError") $
                 vBox
                   [ txt ("│ ✗ error: " <> T.take 120 (resultStderr tr))
                   , txt "╰─"
                   ]
          else emptyWidget
      , if T.null (resultStdout tr) && T.null (resultStderr tr)
          then withAttr (attrName "toolResult") $ txt "╰─ ✓ (empty output)"
          else emptyWidget
      , case resultArtifactPath tr of
          Just p  -> withAttr (attrName "artifact") (txtWrap ("╰─ artifact -> " <> T.pack p))
          Nothing -> emptyWidget
      ]

formatToolCallArgs :: ToolCall -> Text
formatToolCallArgs tc =
  let rawVal = toolCallArgs tc
      parsedVal = case rawVal of
        Aeson.String s -> case Aeson.decode (BL.fromStrict (TE.encodeUtf8 s)) of
          Just (Aeson.Object obj) -> Aeson.Object obj
          _                       -> rawVal
        _ -> rawVal
  in case parsedVal of
    Aeson.Object obj ->
      let formatPair (k, v) =
            let kTxt = K.toText k
                valTxt = case v of
                  Aeson.String strVal ->
                    let clean = T.take 60 (T.replace "\n" " " (T.replace "\r" "" strVal))
                    in "\"" <> clean <> (if T.length strVal > 60 then "…" else "") <> "\""
                  Aeson.Number n -> T.pack (show n)
                  Aeson.Bool b   -> if b then "true" else "false"
                  Aeson.Null     -> "null"
                  _              -> T.take 40 (TE.decodeUtf8 (BL.toStrict (Aeson.encode v)))
            in kTxt <> "=" <> valTxt
      in T.intercalate " " (map formatPair (KM.toList obj))
    Aeson.String s | not (T.null (T.strip s)) ->
      T.take 80 (T.replace "\n" " " (T.replace "\r" "" s))
    _ -> ""

renderToolResultStdout :: Text -> Widget ResourceName
renderToolResultStdout txtContent =
  let ls = T.lines txtContent
      isDiff = any (\l -> "--- " `T.isPrefixOf` l || "+++ " `T.isPrefixOf` l) ls
  in if isDiff
       then vBox
              [ withAttr (attrName "toolResult") (txt "│ diff:")
              , vBox (map renderDiffLine ls)
              , withAttr (attrName "toolResult") (txt "╰─ ✓ diff applied")
              ]
       else case ls of
         [] -> withAttr (attrName "toolResult") (txt "╰─ ✓ (empty output)")
         [single] ->
           withAttr (attrName "toolResult") $
             txtWrap $ "╰─ ✓ " <> T.take 120 single
         _ ->
           let preview = take 12 ls
               extraCount = length ls - length preview
               renderedPreview = map (\l -> withAttr (attrName "toolResult") (txt ("│  " <> T.take 120 l))) preview
               summaryLine = if extraCount > 0
                               then [withAttr (attrName "thinkingDim") (txt ("│  ... (" <> T.pack (show extraCount) <> " more lines)"))]
                               else []
               footer = "╰─ ✓ (" <> T.pack (show (length ls)) <> " lines)"
           in vBox (renderedPreview ++ summaryLine ++ [withAttr (attrName "toolResult") (txt footer)])

renderDiffLine :: Text -> Widget ResourceName
renderDiffLine l
  | "--- " `T.isPrefixOf` l || "+++ " `T.isPrefixOf` l =
      withAttr (attrName "diffHeader") (txt ("│  " <> l))
  | "+" `T.isPrefixOf` l =
      withAttr (attrName "diffAdd") (txt ("│  " <> l))
  | "-" `T.isPrefixOf` l =
      withAttr (attrName "diffRemove") (txt ("│  " <> l))
  | otherwise =
      withAttr (attrName "toolResult") (txt ("│  " <> l))

renderInlineSubAgent :: SubAgentTask -> Widget ResourceName
renderInlineSubAgent task = vLimit 1 (renderTaskChip Nothing task)

renderFleetStrip :: Maybe Int -> [SubAgentTask] -> Widget ResourceName
renderFleetStrip mSelected tasks =
  padTop (Pad 1) $
    padLeft (Pad 2) $
      vLimit 1 $
        hBox
          [ withAttr (attrName "thinkingDim") (str "◈ SubAgents: ")
          , hBox (intersperse (str "  ") (map (renderTaskChip mSelected) tasks))
          , vLimit 1 (fill ' ')
          ]

renderTaskChip :: Maybe Int -> SubAgentTask -> Widget ResourceName
renderTaskChip mSelected SubAgentTask{..} =
  clickable (SubAgentItem subAgentId) $
    let isSelected = mSelected == Just subAgentId
        glyph = if isSelected then "▶" else statusGlyph subAgentStatus
        roleShort = T.take 12 (T.toUpper subAgentRole)
        txtStr = "[" <> glyph <> " #" <> show subAgentId <> " " <> T.unpack roleShort <> " " <> show subAgentTurnCount <> "/" <> show subAgentBudget <> "t]"
        attr = if isSelected then attrName "selectedBadge" else statusAttr subAgentStatus
    in withAttr attr (str txtStr)

statusGlyph :: SubAgentStatus -> String
statusGlyph SubAgentRunning     = "●"
statusGlyph (SubAgentSuccess _) = "✓"
statusGlyph (SubAgentBlocked _) = "▲"
statusGlyph (SubAgentFailed _)  = "✗"

renderCompletionLine :: Maybe CompletionState -> Widget ResourceName
renderCompletionLine Nothing = emptyWidget
renderCompletionLine (Just (CompletionState cands sel))
  | null cands = emptyWidget
  | otherwise =
      let (winSel, visibleCands, overflow) = slidingCandidateWindow sel cands
          renderedItems = zipWith (renderCandidate winSel) [0..] visibleCands
          overflowWidget =
            if overflow > 0
              then [str " ", withAttr (attrName "thinkingDim") (str $ "(+" <> show overflow <> " more)")]
              else []
      in padTop (Pad 1) $
           padLeft (Pad 2) $
             hBox
               ( [ withAttr (attrName "thinkingDim") (str "candidates: ")
                 , hBox (intersperse (str "  ") renderedItems)
                 ] ++ overflowWidget
               )
  where
    renderCandidate selIdx idx cand =
      if idx == selIdx
        then withAttr (attrName "compSelected") (txt ("[" <> candDisplay cand <> "]"))
        else withAttr (attrName "compItem") (txt (candDisplay cand))

-- | Splits a single line into visual lines of custom widths for first slice and rest slices.
wrapLineWithWidths :: Int -> Int -> Text -> [(Text, Int, Int)]
wrapLineWithWidths firstW restW line
  | T.null line = [("", 0, 0)]
  | otherwise =
      let go isFirst offset remText
            | T.null remText = []
            | otherwise =
                let w = if isFirst then max 10 firstW else max 10 restW
                    len = T.length remText
                in if len <= w
                     then [(remText, offset, offset + len)]
                     else
                       let prefix = T.take w remText
                       in case T.findIndex (== ' ') (T.reverse prefix) of
                            Just revSpaceIdx | revSpaceIdx < w - 1 ->
                              let cutPoint = w - revSpaceIdx
                                  slice = T.take cutPoint remText
                                  rest = T.drop cutPoint remText
                              in (slice, offset, offset + cutPoint) : go False (offset + cutPoint) rest
                            _ ->
                              let slice = prefix
                                  rest = T.drop w remText
                              in (slice, offset, offset + w) : go False (offset + w) rest
      in go True 0 line

-- | Splits a single line into visual lines of at most `maxW` width, using word wrapping
-- when spaces exist, or character slicing for long tokens.
wrapLineWithSpans :: Int -> Text -> [(Text, Int, Int)]
wrapLineWithSpans maxW = wrapLineWithWidths maxW maxW

-- | Wraps all logical lines in an editor with separate first-line and rest-line widths,
-- mapping the logical 2D cursor position (curCol, curRow) onto visual lines.
wrapEditorToVisualWithFirstWidth :: Int -> Int -> [Text] -> (Int, Int) -> ([Text], (Int, Int))
wrapEditorToVisualWithFirstWidth firstW restW rawLines (curCol, curRow) =
  let linesToWrap = if null rawLines then [""] else rawLines
      clampedCurRow = max 0 (min (length linesToWrap - 1) curRow)
      
      processLine rowIdx l =
        let spans = if rowIdx == 0
                      then wrapLineWithWidths firstW restW l
                      else wrapLineWithSpans restW l
            lastEnd = case reverse spans of
              ((_, _, e):_) -> e
              []            -> 0
            currSpanW = if rowIdx == 0 && null (drop 1 spans) then firstW else restW
            adjustedSpans =
              if rowIdx == clampedCurRow && curCol > 0 && curCol == lastEnd && (curCol `mod` max 10 currSpanW == 0)
                then spans ++ [("", lastEnd, lastEnd)]
                else spans
            
            findPos [] = (0, 0)
            findPos (s:ss) =
              let (_, start, end) = s
              in if null ss || curCol < end
                   then (max 0 (curCol - start), 0)
                   else let (c, r) = findPos ss in (c, r + 1)
                   
            mCurPos = if rowIdx == clampedCurRow then Just (findPos adjustedSpans) else Nothing
        in (map (\(t, _, _) -> t) adjustedSpans, mCurPos, length adjustedSpans)
        
      results = zipWith processLine [0..] linesToWrap
      allVisualLines = concat [ vs | (vs, _, _) <- results ]
      
      findGlobalCursor _ [] = (0, 0)
      findGlobalCursor accRow ((_, Just (vc, vr), _):_) = (vc, accRow + vr)
      findGlobalCursor accRow ((_, Nothing, count):rest) = findGlobalCursor (accRow + count) rest
      
      visCursor = findGlobalCursor 0 results
  in (allVisualLines, visCursor)

-- | Wraps all logical lines in an editor into visual lines of uniform width maxW
wrapEditorToVisual :: Int -> [Text] -> (Int, Int) -> ([Text], (Int, Int))
wrapEditorToVisual maxW = wrapEditorToVisualWithFirstWidth maxW maxW

-- | Formats elapsed seconds into human-readable duration
formatElapsed :: Int -> Text
formatElapsed secs =
  let m = secs `div` 60
      s = secs `mod` 60
      h = m `div` 60
      remM = m `mod` 60
  in if h > 0
       then T.pack (show h) <> "h " <> T.pack (show remM) <> "m " <> T.pack (show s) <> "s"
       else if m > 0
         then T.pack (show m) <> "m " <> T.pack (show s) <> "s"
         else T.pack (show s) <> "s"

renderPowerlinePrompt :: AgentMode -> Text -> Text -> [Turn] -> Int -> E.Editor Text ResourceName -> Bool -> Int -> Widget ResourceName
renderPowerlinePrompt mode model baseUrl turns ctxLimit editor isWaiting spinnerStep =
  renderPowerlinePromptWithElapsed mode model baseUrl turns ctxLimit editor isWaiting spinnerStep Nothing

renderPowerlinePromptWithElapsed :: AgentMode -> Text -> Text -> [Turn] -> Int -> E.Editor Text ResourceName -> Bool -> Int -> Maybe Text -> Widget ResourceName
renderPowerlinePromptWithElapsed mode model baseUrl turns ctxLimit editor isWaiting spinnerStep mElapsed =
  Widget Greedy Fixed $ do
    c <- getContext
    let totalW = c ^. availWidthL
        spinnerFrames = ["⠋ ", "⠙ ", "⠹ ", "⠸ ", "⠼ ", "⠴ ", "⠦ ", "⠧ ", "⠇ ", "⠏ "]
        promptArrowStr =
          if isWaiting
            then case mElapsed of
                   Just el -> (spinnerFrames !! (spinnerStep `mod` length spinnerFrames)) <> "[" <> T.unpack el <> "] "
                   Nothing -> spinnerFrames !! (spinnerStep `mod` length spinnerFrames)
            else "→ "
        arrowW = max 2 (textVisualWidth (T.pack promptArrowStr))
        firstW = max 10 (totalW - arrowW)
        restW = max 10 totalW
        edContents = E.getEditContents editor
        -- Brick's getCursorPosition returns (row, col)
        (curRow, curCol) = E.getCursorPosition editor
        (visualLines, (visCol, visRow)) =
          wrapEditorToVisualWithFirstWidth firstW restW edContents (curCol, curRow)
        visHeight = min 6 (max 1 (length visualLines))
        screenCursorLoc =
          Location (if visRow == 0 then visCol + arrowW else visCol, visRow)
        renderVisualLine :: Int -> Text -> Widget ResourceName
        renderVisualLine 0 l =
          hBox
            [ withAttr (attrName "promptArrow") (str promptArrowStr)
            , txt (if T.null l then " " else l)
            ]
        renderVisualLine _ l =
          txt (if T.null l then " " else l)
        edWidget =
          vLimit visHeight $
            viewport EditorInput Vertical $
              showCursor EditorInput screenCursorLoc $
                vBox (zipWith renderVisualLine [0..] visualLines)
        topBadgeLine = renderStarshipBadgeLine mode model baseUrl turns ctxLimit totalW
    render $
      padTop (Pad 1) $
        vBox
          [ topBadgeLine
          , edWidget
          ]
  where
    renderStarshipBadgeLine curMode curModel curBaseUrl curTurns curLimit totalW =
      let currentTokens = estimateTotalTokens curTurns
          pct = if curLimit <= 0 then 0 else (currentTokens * 100) `div` curLimit
          (providerStr, modelStr) = extractProviderAndModel curBaseUrl curModel

          slantGlyph    = "\xE0B8"
          leftCapGlyph  = "\xE0B6"
          rightCapGlyph = "\xE0B4"

          capLeft = withAttr (attrName "promptCapLeft") (txt leftCapGlyph)

          seg1 = withAttr (attrName "promptSeg1") (txt " λ ")
          div1to2 = withAttr (attrName "promptDiv1to2") (txt slantGlyph)
          div1to3 = withAttr (attrName "promptDiv1to3") (txt slantGlyph)

          seg2 = withAttr (attrName "promptSeg2") (txt (" " <> providerStr <> " "))
          div2to3 = withAttr (attrName "promptDiv2to3") (txt slantGlyph)

          seg3 = withAttr (attrName "promptSeg3") (txt (" " <> modelStr <> " "))
          div3to5 = case curMode of
            PlanMode -> withAttr (attrName "promptDiv3to5Plan") (txt slantGlyph)
            ExecMode -> withAttr (attrName "promptDiv3to5Exec") (txt slantGlyph)

          (seg4Attr, div3to4Attr, div4to5PlanAttr, div4to5ExecAttr)
            | pct >= 90 = (attrName "promptSeg4High", attrName "promptDiv3to4High", attrName "promptDiv4to5PlanHigh", attrName "promptDiv4to5ExecHigh")
            | pct >= 70 = (attrName "promptSeg4Warn", attrName "promptDiv3to4Warn", attrName "promptDiv4to5PlanWarn", attrName "promptDiv4to5ExecWarn")
            | otherwise = (attrName "promptSeg4Normal", attrName "promptDiv3to4Normal", attrName "promptDiv4to5PlanNormal", attrName "promptDiv4to5ExecNormal")
          seg4 = withAttr seg4Attr (txt (" " <> T.pack (show pct) <> "% "))
          div3to4 = withAttr div3to4Attr (txt slantGlyph)
          div4to5 = case curMode of
            PlanMode -> withAttr div4to5PlanAttr (txt slantGlyph)
            ExecMode -> withAttr div4to5ExecAttr (txt slantGlyph)

          (seg5, capRight) = case curMode of
            PlanMode ->
              ( withAttr (attrName "promptSeg5Plan") (txt " plan ")
              , withAttr (attrName "promptCapRightPlan") (txt rightCapGlyph)
              )
            ExecMode ->
              ( withAttr (attrName "promptSeg5Exec") (txt " exec ")
              , withAttr (attrName "promptCapRightExec") (txt rightCapGlyph)
              )

          segments
            | totalW < 45 = [capLeft, seg1, div1to3, seg3, div3to5, seg5, capRight]
            | totalW < 65 = [capLeft, seg1, div1to2, seg2, div2to3, seg3, div3to5, seg5, capRight]
            | otherwise   = [capLeft, seg1, div1to2, seg2, div2to3, seg3, div3to4, seg4, div4to5, seg5, capRight]
      in hBox segments


renderSubAgents :: Map.Map Int SubAgentTask -> Widget ResourceName
renderSubAgents = renderSubAgentsSelected Nothing

renderSubAgentsSelected :: Maybe Int -> Map.Map Int SubAgentTask -> Widget ResourceName
renderSubAgentsSelected mSelected subs
  | Map.null subs = padAll 1 (withAttr (attrName "thinkingDim") $ str "No active subagents")
  | otherwise = vBox $ map renderTask (Map.elems subs)
  where
    renderTask SubAgentTask{..} =
      let isSelected = mSelected == Just subAgentId
          roleUpper = T.unpack (T.toUpper subAgentRole)
          taskWidget =
            padBottom (Pad 1) $
              vBox
                [ vLimit 1 $ hBox
                    [ withAttr (attrName "subRole") (str $ "[" <> roleUpper <> "]")
                    , str $ " #" <> show subAgentId
                    , str " "
                    , withAttr (statusAttr subAgentStatus) (str $ "[" <> formatStatus subAgentStatus <> "]")
                    , if isSelected
                        then withAttr (attrName "selectedBadge") (str " [VIEWING]")
                        else emptyWidget
                    , vLimit 1 (fill ' ')
                    , str $ show subAgentTurnCount <> "/" <> show subAgentBudget <> "t"
                    ]
                , padLeft (Pad 1) $ txtWrap (T.take 50 subAgentHypothesis)
                , case subAgentArtifact of
                    Just p  -> padLeft (Pad 1) $ withAttr (attrName "artifact") (str $ "-> " <> take 26 p)
                    Nothing -> emptyWidget
                ]
      in clickable (SubAgentItem subAgentId) taskWidget

formatStatus :: SubAgentStatus -> String
formatStatus SubAgentRunning                 = "RUNNING"
formatStatus (SubAgentSuccess "INCONCLUSIVE") = "EXHAUSTED"
formatStatus (SubAgentSuccess _)             = "SUCCESS"
formatStatus (SubAgentBlocked _)             = "BLOCKED"
formatStatus (SubAgentFailed _)              = "FAILED"

statusAttr :: SubAgentStatus -> AttrName
statusAttr SubAgentRunning                 = attrName "subRunning"
statusAttr (SubAgentSuccess "INCONCLUSIVE") = attrName "warning"
statusAttr (SubAgentSuccess _)             = attrName "subSuccess"
statusAttr _                               = attrName "subFailed"
