{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Draw
  ( drawApp
  , renderSubAgents
  , renderSubAgentsSelected
  , renderInlineSubAgent
  , renderCompletionLine
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Time.Format (defaultTimeLocale, formatTime)

import Lambda.Config (Config(..), formatEndpointBadge)
import Lambda.Engine.Compactor (estimateTotalTokens)
import Lambda.Engine.Session (SessionMeta(..))
import Lambda.Types
import Lambda.UI.Completion (slidingCandidateWindow)
import Lambda.UI.Markdown (renderMarkdown)
import Lambda.UI.Types

drawApp :: UIState -> [Widget ResourceName]
drawApp st = [modalOverlay st, sessionChooserOverlay st, hudOverlay st, mainLayout st]

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

mainLayout :: UIState -> Widget ResourceName
mainLayout UIState{..} =
  vBox
    [ headerBar uiWorkingState uiSelectedSubAgent
    , renderMainPane uiSelectedSubAgent currentDisplayTurns uiSubAgents uiCompletion uiMode uiModelName uiContextLimit uiConfig uiEditor
    ]
  where
    currentDisplayTurns = case uiSelectedSubAgent of
      Just sId -> case Map.lookup sId uiSubAgents of
        Just task -> subAgentTurns task
        Nothing   -> uiTurns
      Nothing  -> uiTurns

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
  -> Widget ResourceName
renderMainPane Nothing turns subs uiComp mode model ctxLimit cfg editor =
  vBox
    [ viewport ChatView Vertical $
        padLeftRight 1 $
          vBox
            [ vBox (map renderTurn turns)
            , if Map.null subs
                then emptyWidget
                else padTop (Pad 1) $ vBox (map renderInlineSubAgent (Map.elems subs))
            ]
    , renderCompletionLine uiComp
    , renderPowerlinePrompt mode model (apiBaseUrl cfg) turns ctxLimit editor
    ]
renderMainPane (Just sId) _ subs uiComp mode model ctxLimit cfg editor =
  case Map.lookup sId subs of
    Just task ->
      let roleUpper = T.unpack (T.toUpper (subAgentRole task))
          banner = withAttr (attrName "selectedBadge") $
            str $ " ◄ SUBAGENT #" <> show sId <> " [" <> roleUpper <> "] DIALOGUE (Press Esc to return to Main Chat) ► "
          renderedTurns =
            if null (subAgentTurns task)
              then padAll 1 (withAttr (attrName "thinkingDim") $ str "No dialogue turns recorded for this subagent yet.")
              else vBox (map renderTurn (subAgentTurns task))
      in vBox
           [ viewport ChatView Vertical $
               padLeftRight 1 $
                 vBox
                   [ C.hCenter banner
                   , renderedTurns
                   ]
           , renderCompletionLine uiComp
           , renderPowerlinePrompt mode model (apiBaseUrl cfg) (subAgentTurns task) ctxLimit editor
           ]
    Nothing ->
      viewport ChatView Vertical (padAll 1 $ withAttr (attrName "toolError") $ str ("SubAgent #" <> show sId <> " not found."))

headerBar :: Text -> Maybe Int -> Widget ResourceName
headerBar workingState mSelected =
  vLimit 1 $
    withAttr (attrName "headerBar") $
      hBox
        [ withAttr (attrName "titleLogo") (str " λ lambdA ")
        , str " │ "
        , str "Goal: "
        , txt (extractGoal workingState)
        , case mSelected of
            Just sId -> hBox [ str " │ ", withAttr (attrName "selectedBadge") (str $ " [SUBAGENT #" <> show sId <> "] ") ]
            Nothing  -> emptyWidget
        , fill ' '
        , txt (extractBlocked workingState)
        , str " "
        ]
  where
    extractGoal st =
      case filter ("GOAL:" `T.isPrefixOf`) (T.lines st) of
        (g:_) -> T.strip (T.drop 5 g)
        []    -> "Awaiting task"
    extractBlocked st =
      case filter ("BLOCKED_ON:" `T.isPrefixOf`) (T.lines st) of
        (b:_) -> "● " <> T.strip b
        []    -> "✓ Ready"

renderTurn :: Turn -> Widget ResourceName
renderTurn (Turn _ UserRole blocks) =
  padBottom (Pad 1) $
    hBox
      [ withAttr (attrName "userAccent") (str "▎ ")
      , vBox (map renderBlock blocks)
      ]
renderTurn (Turn tId role blocks) =
  padBottom (Pad 1) $
    vBox
      [ if role == SystemRole
          then withAttr (attrName "systemRole") (str $ "● system (Turn #" <> show tId <> ")")
          else emptyWidget
      , vBox (map renderBlock blocks)
      , if role == AssistantRole
          then padLeft (Pad 2) $ withAttr (attrName "turnFooter") (str "■ Plan · assistant")
          else emptyWidget
      ]

renderBlock :: ContentBlock -> Widget ResourceName
renderBlock (TextBlock t) =
  padLeft (Pad 2) $ renderMarkdown t
renderBlock (ThinkingBlock tId body vis) =
  clickable (ThinkingFold tId) $
    padLeft (Pad 2) $
      case vis of
        Collapsed -> withAttr (attrName "thinkingDim") $ str "▶ Thought (press ^T or click to expand)"
        Visible   ->
          withAttr (attrName "thinkingDim") $
            txtWrap ("Thinking: " <> body)
renderBlock (ToolCallBlock tc) =
  padLeft (Pad 2) $
    withAttr (attrName "toolCall") $
      let formattedArgs = TE.decodeUtf8 (BL.toStrict (Aeson.encode (toolCallArgs tc)))
      in txtWrap $ "⚡ call: " <> toolCallName tc <> " " <> T.take 100 formattedArgs
renderBlock (ToolResultBlock tr) =
  padLeft (Pad 2) $
    vBox
      [ if not (T.null (resultStdout tr))
          then renderToolResultStdout (resultStdout tr)
          else emptyWidget
      , if not (T.null (resultStderr tr))
          then withAttr (attrName "toolError") $ txtWrap $ "✗ error: " <> T.take 120 (resultStderr tr)
          else emptyWidget
      , if T.null (resultStdout tr) && T.null (resultStderr tr)
          then withAttr (attrName "toolResult") $ txtWrap "✓ (empty output)"
          else emptyWidget
      , case resultArtifactPath tr of
          Just p  -> withAttr (attrName "artifact") (txtWrap ("  artifact -> " <> T.pack p))
          Nothing -> emptyWidget
      ]

renderToolResultStdout :: Text -> Widget ResourceName
renderToolResultStdout txtContent =
  let ls = T.lines txtContent
      isDiff = any (\l -> "--- " `T.isPrefixOf` l || "+++ " `T.isPrefixOf` l) ls
  in if not isDiff
       then withAttr (attrName "toolResult") $ txtWrap $ "✓ result: " <> T.take 120 txtContent
       else vBox (map renderDiffLine ls)

renderDiffLine :: Text -> Widget ResourceName
renderDiffLine l
  | "--- " `T.isPrefixOf` l || "+++ " `T.isPrefixOf` l =
      withAttr (attrName "diffHeader") (txt l)
  | "+" `T.isPrefixOf` l =
      withAttr (attrName "diffAdd") (txt l)
  | "-" `T.isPrefixOf` l =
      withAttr (attrName "diffRemove") (txt l)
  | otherwise =
      withAttr (attrName "toolResult") (txt l)

renderInlineSubAgent :: SubAgentTask -> Widget ResourceName
renderInlineSubAgent SubAgentTask{..} =
  clickable (SubAgentItem subAgentId) $
    padBottom (Pad 1) $
      withBorderStyle BS.unicodeRounded $
        B.border $
          padLeftRight 1 $
            vBox
              [ vLimit 1 $ hBox
                  [ withAttr (attrName "subRole") (str $ "◆ SubAgent #" <> show subAgentId <> " [" <> T.unpack (T.toUpper subAgentRole) <> "]")
                  , str " "
                  , withAttr (statusAttr subAgentStatus) (str $ "[" <> formatStatus subAgentStatus <> "]")
                  , vLimit 1 (fill ' ')
                  , withAttr (attrName "thinkingDim") (str $ show subAgentTurnCount <> "/" <> show subAgentBudget <> "t  [click or /sub " <> show subAgentId <> "]")
                  ]
              , padLeft (Pad 2) $ withAttr (attrName "thinkingDim") $ txtWrap (T.take 80 subAgentHypothesis)
              , case subAgentArtifact of
                  Just p  -> padLeft (Pad 2) $ withAttr (attrName "artifact") (str $ "-> artifact: " <> take 40 p)
                  Nothing -> emptyWidget
              ]

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

renderPowerlinePrompt :: AgentMode -> Text -> Text -> [Turn] -> Int -> E.Editor Text ResourceName -> Widget ResourceName
renderPowerlinePrompt mode model baseUrl turns ctxLimit editor =
  padTop (Pad 1) $
    hBox
      [ withAttr (attrName "promptLogo") (str " λ ")
      , withAttr (attrName "promptDivider") (str " \\ ")
      , modelWidget
      , withAttr (attrName "promptDivider") (str " \\ ")
      , withAttr ctxAttr (str (show pct <> "%"))
      , withAttr (attrName "promptDivider") (str " \\ ")
      , modeWidget mode
      , withAttr (attrName "promptArrow") (str " > ")
      , E.renderEditor (txt . T.unlines) True editor
      ]
  where
    currentTokens = estimateTotalTokens turns
    pct = if ctxLimit <= 0 then 0 else (currentTokens * 100) `div` ctxLimit
    ctxAttr
      | pct >= 90 = attrName "ctxHigh"
      | pct >= 70 = attrName "ctxWarn"
      | otherwise = attrName "ctxNormal"
    badge = formatEndpointBadge baseUrl
    modelWidget = case () of
      _ | T.null baseUrl && T.null model ->
            withAttr (attrName "toolError") (txt "NO PROVIDER CONFIGURED")
      _ | T.null baseUrl ->
            withAttr (attrName "toolError") (txt "NO BASE_URL")
      _ | T.null model ->
            withAttr (attrName "thinkingDim") (txt (badge <> "NO MODEL"))
      _ ->
            withAttr (attrName "promptModel") (txt (badge <> model))
    modeWidget PlanMode = withAttr (attrName "promptPlanMode") (str " plan ")
    modeWidget ExecMode = withAttr (attrName "promptExecMode") (str " exec ")


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
formatStatus SubAgentRunning      = "RUNNING"
formatStatus (SubAgentSuccess _)  = "SUCCESS"
formatStatus (SubAgentBlocked _)  = "BLOCKED"
formatStatus (SubAgentFailed _)   = "FAILED"

statusAttr :: SubAgentStatus -> AttrName
statusAttr SubAgentRunning     = attrName "subRunning"
statusAttr (SubAgentSuccess _) = attrName "subSuccess"
statusAttr _                   = attrName "subFailed"
