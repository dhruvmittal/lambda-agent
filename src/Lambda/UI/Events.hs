{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Events
  ( handleAppEvent
  ) where

import Brick
import qualified Brick.Widgets.Edit as E
import Control.Concurrent.STM (atomically, writeTBQueue)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Lambda.Core.EngineInterface (EngineChannels(..))
import Lambda.Engine.Security (resolvePrompt)
import Lambda.Types
import Lambda.UI.Types

handleAppEvent :: BrickEvent ResourceName EngineEvent -> EventM ResourceName UIState ()
handleAppEvent (AppEvent engineEv) = do
  case engineEv of
    EvTurnAdded turn -> do
      modify $ \s -> s { uiTurns = uiTurns s ++ [turn] }
      vScrollToEnd (viewportScroll ChatView)
    EvTurnUpdated turn -> do
      modify $ \s ->
        s { uiTurns = map (\t -> if turnId t == turnId turn then turn else t) (uiTurns s) }
      vScrollToEnd (viewportScroll ChatView)
    EvSubAgentUpdate task -> modify $ \s ->
      s { uiSubAgents = Map.insert (subAgentId task) task (uiSubAgents s) }
    EvWorkingStateUpdate ws -> modify $ \s ->
      s { uiWorkingState = ws }
    EvPermissionRequired prompt -> modify $ \s ->
      case uiCurrentPrompt s of
        Nothing -> s { uiCurrentPrompt = Just prompt }
        Just _  -> s { uiPendingPrompts = uiPendingPrompts s |> prompt }
    EvPermissionResolved _ _ -> pure ()
    EvError err -> do
      tId <- gets (succ . length . uiTurns)
      modify $ \s -> s { uiTurns = uiTurns s ++ [Turn tId SystemRole [TextBlock ("⚠️ Error: " <> err)]] }
      vScrollToEnd (viewportScroll ChatView)
    EvStreamChunk _ -> pure ()

-- Keystrokes when permission modal is open
handleAppEvent (VtyEvent (V.EvKey (V.KChar '1') [])) = resolveActiveModal PermAlways
handleAppEvent (VtyEvent (V.EvKey (V.KChar '2') [])) = resolveActiveModal PermOnce
handleAppEvent (VtyEvent (V.EvKey (V.KChar '3') [])) = resolveActiveModal PermNo
handleAppEvent (VtyEvent (V.EvKey (V.KChar '4') [])) = resolveActiveModal PermNever

-- Mouse clicks on modal buttons
handleAppEvent (MouseDown ButtonAlways V.BLeft _ _) = resolveActiveModal PermAlways
handleAppEvent (MouseDown ButtonOnce   V.BLeft _ _) = resolveActiveModal PermOnce
handleAppEvent (MouseDown ButtonNo     V.BLeft _ _) = resolveActiveModal PermNo
handleAppEvent (MouseDown ButtonNever  V.BLeft _ _) = resolveActiveModal PermNever

-- Mouse click on thinking accordion fold header
handleAppEvent (MouseDown (ThinkingFold tId) V.BLeft _ _) =
  modify $ \s -> s { uiTurns = map (toggleThinking tId) (uiTurns s) }
  where
    toggleThinking targetId t@(Turn _ _ blks) =
      t { turnBlocks = map (flipVis targetId) blks }
    flipVis targetId (ThinkingBlock i b vis)
      | i == targetId = ThinkingBlock i b (if vis == Visible then Collapsed else Visible)
    flipVis _ other = other

-- Mouse wheel scrolling
handleAppEvent (MouseDown ChatView V.BScrollUp _ _) =
  vScrollBy (viewportScroll ChatView) (-3)
handleAppEvent (MouseDown ChatView V.BScrollDown _ _) =
  vScrollBy (viewportScroll ChatView) 3
handleAppEvent (MouseDown SubAgentView V.BScrollUp _ _) =
  vScrollBy (viewportScroll SubAgentView) (-3)
handleAppEvent (MouseDown SubAgentView V.BScrollDown _ _) =
  vScrollBy (viewportScroll SubAgentView) 3

-- Keyboard Enter: Submit prompt or dispatch slash command
handleAppEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  let rawLines = E.getEditContents (uiEditor st)
      inputText = T.strip (T.unlines rawLines)
  if T.null inputText
    then pure ()
    else do
      -- Reset input editor
      put st { uiEditor = E.editor EditorInput (Just 1) "" }
      handleCommand inputText

-- Default text editor input
handleAppEvent (VtyEvent ev) = do
  zoom uiEditorLens (E.handleEditorEvent (VtyEvent ev))

handleAppEvent _ = pure ()

handleCommand :: Text -> EventM ResourceName UIState ()
handleCommand cmdText = do
  st <- get
  let channels = uiChannels st
  case cmdText of
    "/help" -> do
      let helpText = T.unlines
            [ "lambdA Commands & Controls:"
            , "  /plan          - Switch to Plan mode (read-only tools, no mutations)"
            , "  /exec          - Switch to Exec mode (full tools: bash, file writes)"
            , "  /compact       - Trigger manual context compaction to disk archives"
            , "  /clear         - Clear conversation history"
            , "  /help          - Show this help reference"
            , "  /quit          - Exit application"
            , ""
            , "Shortcuts & Navigation:"
            , "  Keys 1,2,3,4   - Resolve authorization prompt (Always, Once, No, Never)"
            , "  Mouse Left     - Click modal buttons / toggle thinking accordions"
            , "  Mouse Wheel    - Scroll Chat and SubAgents views"
            ]
      tId <- gets (succ . length . uiTurns)
      modify $ \s -> s { uiTurns = uiTurns s ++ [Turn tId SystemRole [TextBlock helpText]] }
      vScrollToEnd (viewportScroll ChatView)
    "/plan" -> do
      put st { uiMode = PlanMode }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSetMode PlanMode)
    "/exec" -> do
      put st { uiMode = ExecMode }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSetMode ExecMode)
    "/compact" -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdCompactHistory
    "/clear" -> do
      put st { uiTurns = [] }
    "/quit" -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdQuit
      halt
    _ -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdUserPrompt cmdText)

resolveActiveModal :: PermissionLevel -> EventM ResourceName UIState ()
resolveActiveModal level = do
  st <- get
  case uiCurrentPrompt st of
    Just p -> do
      liftIO $ resolvePrompt p level
      -- Pop next pending prompt from queue if any
      case uiPendingPrompts st of
        nextP :<| rest -> put st { uiCurrentPrompt = Just nextP, uiPendingPrompts = rest }
        Seq.Empty      -> put st { uiCurrentPrompt = Nothing }
    Nothing -> pure ()

-- Lens helper for editor zooming
uiEditorLens :: Functor f => (E.Editor Text ResourceName -> f (E.Editor Text ResourceName)) -> UIState -> f UIState
uiEditorLens f s = (\e -> s { uiEditor = e }) <$> f (uiEditor s)
