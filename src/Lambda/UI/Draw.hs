{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Draw
  ( drawApp
  ) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

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
    [ headerBar uiMode
    , hBox
        [ -- Left Pane: Conversation turns (flexibly occupies remaining terminal width)
          B.borderWithLabel (withAttr (attrName "paneTitle") (str " Conversation ")) $
            viewport ChatView Vertical (padLeftRight 1 (vBox (map renderTurn uiTurns)))
        , -- Right Pane: Fixed 36-column sidebar for state & subagents
          hLimit 36 $
            vBox
              [ vLimitPercent 45 $
                  B.borderWithLabel (withAttr (attrName "paneTitle") (str " State Vector ")) $
                    padLeftRight 1 (renderStateVector uiWorkingState)
              , B.borderWithLabel (withAttr (attrName "paneTitle") (str " SubAgents ")) $
                  viewport SubAgentView Vertical (renderSubAgents uiSubAgents)
              ]
        ]
    , -- Bottom Pane: Dedicated 3-row command editor
      vLimit 3 $
        B.borderWithLabel (inputLabel uiMode) $
          E.renderEditor (txt . T.unlines) True uiEditor
    ]

headerBar :: AgentMode -> Widget ResourceName
headerBar mode =
  vLimit 1 $
    withAttr (attrName "headerBar") $
      hBox
        [ withAttr (attrName "titleLogo") (str " λ lambdA ")
        , str " │ "
        , modeBadge mode
        , str " │ Haskell Systems Engineering Agent "
        , fill ' '
        , str "[1-4: Perms]  [Scroll: Wheel]  [Help: /help] "
        ]

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
        Collapsed -> withAttr (attrName "thinkingDim") $ str "▶ [Thinking] (click header to expand)"
        Visible   ->
          withBorderStyle BS.unicodeRounded $
            B.borderWithLabel (str " Reasoning Scratchpad ") $
              padAll 1 (txtWrap body)
renderBlock (ToolCallBlock tc) =
  padLeft (Pad 2) $
    withAttr (attrName "toolCall") $
      txtWrap $ "⚡ call: " <> toolCallName tc <> " " <> T.pack (take 70 (show (toolCallArgs tc)))
renderBlock (ToolResultBlock tr) =
  padLeft (Pad 2) $
    withAttr (attrName "toolResult") $
      vBox
        [ txtWrap $ "✓ result: " <> T.take 120 (resultStdout tr)
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
renderSubAgents subs
  | Map.null subs = padAll 1 (withAttr (attrName "thinkingDim") $ str "No active subagents")
  | otherwise = vBox $ map renderTask (Map.elems subs)
  where
    renderTask SubAgentTask{..} =
      padBottom (Pad 1) $
        vBox
          [ hBox
              [ withAttr (statusAttr subAgentStatus) (str $ "[" <> formatStatus subAgentStatus <> "]")
              , str $ " #" <> show subAgentId
              , fill ' '
              , str $ show subAgentTurnCount <> "/" <> show subAgentBudget <> "t"
              ]
          , padLeft (Pad 1) $ txtWrap (T.take 50 subAgentHypothesis)
          , case subAgentArtifact of
              Just p  -> padLeft (Pad 1) $ withAttr (attrName "artifact") (str $ "-> " <> take 26 p)
              Nothing -> emptyWidget
          ]

    formatStatus SubAgentRunning      = "RUNNING"
    formatStatus (SubAgentSuccess _)  = "SUCCESS"
    formatStatus (SubAgentBlocked _)  = "BLOCKED"
    formatStatus (SubAgentFailed _)   = "FAILED"

    statusAttr SubAgentRunning     = attrName "subRunning"
    statusAttr (SubAgentSuccess _) = attrName "subSuccess"
    statusAttr _                   = attrName "subFailed"
