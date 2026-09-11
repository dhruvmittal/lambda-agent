{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Draw
  ( drawApp
  , renderSubAgents
  , renderSubAgentsSelected
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Lambda.Engine.Compactor (estimateTotalTokens)
import Lambda.Types
import Lambda.UI.Types

drawApp :: UIState -> [Widget ResourceName]
drawApp st = [modalOverlay st, mainLayout st]

modalOverlay :: UIState -> Widget ResourceName
modalOverlay UIState{ uiCurrentPrompt = Just p } =
  C.centerLayer $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str " Security Gate: Authorization Required ") $
        hLimit 64 $ vLimit 12 $
          padAll 1 $
            vBox
              [ padBottom (Pad 1) $ str "An operation requires user confirmation:"
              , str $ "Caller: " <> show (promptCaller p)
              , str $ "Tool:   " <> T.unpack (promptTool p)
              , padBottom (Pad 1) $ txtWrap ("Args:   " <> T.pack (take 50 (show (promptArgs p))))
              , C.hCenter $
                  hBox
                    [ clickable ButtonAlways $ B.border (str " [1] Always ")
                    , str " "
                    , clickable ButtonOnce   $ B.border (str " [2] Once ")
                    , str " "
                    , clickable ButtonNo     $ B.border (str " [3] No ")
                    , str " "
                    , clickable ButtonNever  $ B.border (str " [4] Never ")
                    ]
              ]
modalOverlay _ = emptyWidget

mainLayout :: UIState -> Widget ResourceName
mainLayout UIState{..} =
  vBox
    [ headerBar uiMode currentDisplayTurns uiContextLimit uiModelName uiSelectedSubAgent
    , hBox
        [ -- Left Pane: Conversation turns or SubAgent Dialogue
          renderLeftPane uiSelectedSubAgent uiTurns uiSubAgents
        , -- Right Pane: Fixed 36-column sidebar for state & subagents
          hLimit 36 $
            vBox
              [ vLimitPercent 45 $
                  B.borderWithLabel (withAttr (attrName "paneTitle") (str " State Vector ")) $
                    padLeftRight 1 (renderStateVector uiWorkingState)
              , B.borderWithLabel (withAttr (attrName "paneTitle") (str " SubAgents ")) $
                  viewport SubAgentView Vertical (renderSubAgentsSelected uiSelectedSubAgent uiSubAgents)
              ]
        ]
    , -- Bottom Pane: Dedicated 3-row command editor
      vLimit 3 $
        B.borderWithLabel (inputLabel uiMode) $
          E.renderEditor (txt . T.unlines) True uiEditor
    ]
  where
    currentDisplayTurns = case uiSelectedSubAgent of
      Just sId -> case Map.lookup sId uiSubAgents of
        Just task -> subAgentTurns task
        Nothing   -> uiTurns
      Nothing  -> uiTurns

renderLeftPane :: Maybe Int -> [Turn] -> Map.Map Int SubAgentTask -> Widget ResourceName
renderLeftPane Nothing turns _ =
  B.borderWithLabel (withAttr (attrName "paneTitle") (str " Conversation ")) $
    viewport ChatView Vertical (padLeftRight 1 (vBox (map renderTurn turns)))
renderLeftPane (Just sId) _ subs =
  case Map.lookup sId subs of
    Just task ->
      let roleUpper = T.unpack (T.toUpper (subAgentRole task))
          title = " SubAgent #" <> show sId <> " [" <> roleUpper <> "] Dialogue [Alt+← Main | Alt+→ Next | Esc Exit] "
          turns = subAgentTurns task
          renderedContent =
            if null turns
              then padAll 1 (withAttr (attrName "thinkingDim") $ str "No dialogue turns recorded for this subagent yet.")
              else vBox (map renderTurn turns)
      in B.borderWithLabel (withAttr (attrName "paneTitle") (str title)) $
           viewport ChatView Vertical (padLeftRight 1 renderedContent)
    Nothing ->
      B.borderWithLabel (withAttr (attrName "paneTitle") (str " SubAgent Dialogue ")) $
        viewport ChatView Vertical (padAll 1 $ withAttr (attrName "toolError") $ str ("SubAgent #" <> show sId <> " not found."))

headerBar :: AgentMode -> [Turn] -> Int -> Text -> Maybe Int -> Widget ResourceName
headerBar mode turns ctxLimit model mSelected =
  vLimit 1 $
    withAttr (attrName "headerBar") $
      hBox
        [ withAttr (attrName "titleLogo") (str " λ lambdA ")
        , str " │ "
        , modeBadge mode
        , case mSelected of
            Just sId -> hBox [ str " │ ", withAttr (attrName "selectedBadge") (str $ " [VIEWING SUBAGENT #" <> show sId <> "] ") ]
            Nothing  -> emptyWidget
        , str " │ "
        , contextBadge
        , str " │ "
        , withAttr (attrName "modelBadge") (str ("Model: " <> T.unpack model))
        , fill ' '
        , str "[Alt+←/→: SubAgents]  [^P/^N: Hist]  [^T: Think]  [Esc Esc: Stop]  [/help] "
        ]
  where
    currentTokens = estimateTotalTokens turns
    pct = if ctxLimit <= 0 then 0 else (currentTokens * 100) `div` ctxLimit
    formatK n
      | n >= 1000 = show (n `div` 1000) <> "." <> show ((n `mod` 1000) `div` 100) <> "k"
      | otherwise = show n
    ctxStr = "Ctx: " <> formatK currentTokens <> " / " <> formatK ctxLimit <> " (" <> show pct <> "%)"
    ctxAttr
      | pct >= 90 = attrName "ctxHigh"
      | pct >= 70 = attrName "ctxWarn"
      | otherwise = attrName "ctxNormal"
    contextBadge = withAttr ctxAttr (str ctxStr)

inputLabel :: AgentMode -> Widget ResourceName
inputLabel PlanMode =
  hBox [ withAttr (attrName "planBadge") (str " PLAN ") , str " [Read-Only] Enter prompt or /help " ]
inputLabel ExecMode =
  hBox [ withAttr (attrName "execBadge") (str " EXEC ") , str " [Full-Access] Enter command or /help " ]

modeBadge :: AgentMode -> Widget ResourceName
modeBadge PlanMode = withAttr (attrName "planBadge") $ str " [PLAN] "
modeBadge ExecMode = withAttr (attrName "execBadge") $ str " [EXEC] "

renderTurn :: Turn -> Widget ResourceName
renderTurn (Turn tId role blocks) =
  padBottom (Pad 1) $
    vBox
      [ withAttr (roleAttr role) (str $ rolePrefix role <> " Turn #" <> show tId)
      , vBox (map renderBlock blocks)
      ]
  where
    roleAttr SystemRole    = attrName "systemRole"
    roleAttr UserRole      = attrName "userRole"
    roleAttr AssistantRole = attrName "assistantRole"
    roleAttr ToolRole      = attrName "toolRole"

    rolePrefix SystemRole    = "● system"
    rolePrefix UserRole      = "● user"
    rolePrefix AssistantRole = "● lambdA"
    rolePrefix ToolRole      = "● tool"

renderBlock :: ContentBlock -> Widget ResourceName
renderBlock (TextBlock t) =
  padLeft (Pad 2) $ txtWrap t
renderBlock (ThinkingBlock tId body vis) =
  padLeft (Pad 2) $
    clickable (ThinkingFold tId) $
      case vis of
        Collapsed -> withAttr (attrName "thinkingDim") $ str "▶ [Thinking] (press ^T or /think to expand)"
        Visible   ->
          withBorderStyle BS.unicodeRounded $
            B.borderWithLabel (str " Reasoning Scratchpad ") $
              padAll 1 (txtWrap body)
renderBlock (ToolCallBlock tc) =
  padLeft (Pad 2) $
    withAttr (attrName "toolCall") $
      let formattedArgs = TE.decodeUtf8 (BL.toStrict (Aeson.encode (toolCallArgs tc)))
      in txtWrap $ "⚡ call: " <> toolCallName tc <> " " <> T.take 100 formattedArgs
renderBlock (ToolResultBlock tr) =
  padLeft (Pad 2) $
    vBox
      [ if not (T.null (resultStdout tr))
          then withAttr (attrName "toolResult") $ txtWrap $ "✓ result: " <> T.take 120 (resultStdout tr)
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

renderStateVector :: Text -> Widget ResourceName
renderStateVector rawText =
  vBox $ map (txtWrap . formatStateLine) (T.lines rawText)
  where
    formatStateLine l
      | "GOAL:" `T.isPrefixOf` l             = "🎯 " <> l
      | "INVARIANTS:" `T.isPrefixOf` l       = "🛡️  " <> l
      | "ACTIVE_HYPOTHESIS:" `T.isPrefixOf` l = "💡 " <> l
      | "BLOCKED_ON:" `T.isPrefixOf` l       = "⏳ " <> l
      | otherwise                            = l

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

    formatStatus SubAgentRunning      = "RUNNING"
    formatStatus (SubAgentSuccess _)  = "SUCCESS"
    formatStatus (SubAgentBlocked _)  = "BLOCKED"
    formatStatus (SubAgentFailed _)   = "FAILED"

    statusAttr SubAgentRunning     = attrName "subRunning"
    statusAttr (SubAgentSuccess _) = attrName "subSuccess"
    statusAttr _                   = attrName "subFailed"
