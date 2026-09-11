{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Events
  ( handleAppEvent
  , allCommands
  , computeCommandMatches
  , replaceCurrentToken
  ) where

import Brick
import qualified Brick.Widgets.Edit as E
import Control.Concurrent.STM (atomically, writeTBQueue)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Zipper as Z
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.Vty as V
import System.Posix.Signals (raiseSignal, sigTSTP)

import Lambda.Core.EngineInterface (EngineChannels(..))
import Lambda.Engine.Security (resolvePrompt)
import Lambda.Engine.Session
  ( SessionMeta(..)
  , listSessions
  , pruneSessions
  )
import Lambda.Types
import Lambda.UI.Completion (completeInput, allCommands, computeCommandCandidates)
import Lambda.UI.Types

handleAppEvent :: BrickEvent ResourceName EngineEvent -> EventM ResourceName UIState ()
handleAppEvent (AppEvent engineEv) = do
  case engineEv of
    EvTurnAdded turn -> do
      vis <- gets uiThinkingVisible
      let adjusted = setThinkingVis (if vis then Visible else Collapsed) turn
          genDone = turnRole turn == AssistantRole
      modify $ \s -> s
        { uiTurns = uiTurns s ++ [adjusted]
        , uiIsGenerating = if genDone then False else uiIsGenerating s
        }
      vScrollToEnd (viewportScroll ChatView)
    EvTurnUpdated turn -> do
      vis <- gets uiThinkingVisible
      let adjusted = setThinkingVis (if vis then Visible else Collapsed) turn
      modify $ \s ->
        s { uiTurns = updateMatchingTurn adjusted (uiTurns s) }
      vScrollToEnd (viewportScroll ChatView)
    EvSessionSwitched _ mode turns subs -> do
      vis <- gets uiThinkingVisible
      let adjustedTurns = map (setThinkingVis (if vis then Visible else Collapsed)) turns
          adjustedSubs = Map.map (\t -> t { subAgentTurns = map (setThinkingVis (if vis then Visible else Collapsed)) (subAgentTurns t) }) subs
      modify $ \s -> s
        { uiTurns = adjustedTurns
        , uiSubAgents = adjustedSubs
        , uiMode = mode
        , uiSelectedSubAgent = Nothing
        , uiIsGenerating = False
        }
      vScrollToEnd (viewportScroll ChatView)
    EvSubAgentUpdate task -> modify $ \s ->
      let vis = uiThinkingVisible s
          adjustedTurns = map (setThinkingVis (if vis then Visible else Collapsed)) (subAgentTurns task)
          adjustedTask = task { subAgentTurns = adjustedTurns }
      in s { uiSubAgents = Map.insert (subAgentId task) adjustedTask (uiSubAgents s) }
    EvWorkingStateUpdate ws -> modify $ \s ->
      s { uiWorkingState = ws }
    EvPermissionRequired prompt -> modify $ \s ->
      case uiCurrentPrompt s of
        Nothing -> s { uiCurrentPrompt = Just prompt }
        Just _  -> s { uiPendingPrompts = uiPendingPrompts s |> prompt }
    EvPermissionResolved _ _ -> pure ()
    EvError err -> do
      tId <- gets (\s -> negate (1000 + length (uiTurns s)))
      modify $ \s -> s
        { uiTurns = uiTurns s ++ [Turn tId SystemRole [TextBlock ("⚠️ Error: " <> err)]]
        , uiIsGenerating = False
        }
      vScrollToEnd (viewportScroll ChatView)
    EvStreamChunk _ -> pure ()
  where
    updateMatchingTurn updatedTurn turns =
      let rev = reverse turns
          go [] = []
          go (t:rest)
            | turnId t == turnId updatedTurn = updatedTurn : rest
            | otherwise                      = t : go rest
      in reverse (go rev)

-- =========================================================================
-- Terminal Conventions & Signal Controls
-- =========================================================================

-- ^C: Cancel line or interrupt active execution
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = do
  st <- get
  let editorText = T.strip (T.concat (E.getEditContents (uiEditor st)))
      hasRunningSubs = any (\t -> subAgentStatus t == SubAgentRunning) (Map.elems (uiSubAgents st))
  if uiIsGenerating st || hasRunningSubs
    then do
      liftIO $ atomically $ do
        writeTBQueue (cmdQueue (uiChannels st)) CmdInterrupt
        writeTBQueue (cmdQueue (uiChannels st)) (CmdSystemMessage "⚠️ Generation interrupted (Ctrl+C)...")
      put st { uiIsGenerating = False }
    else if not (T.null editorText)
      then put st { uiEditor = E.editor EditorInput (Just 1) "", uiCompletion = Nothing }
      else do
        liftIO $ atomically $
          writeTBQueue (cmdQueue (uiChannels st)) (CmdSystemMessage "💡 Press Ctrl+D or type /quit to exit lambdA.")

-- ^D: EOF clean exit on empty line; forward delete character if non-empty
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = do
  st <- get
  let editorContents = T.concat (E.getEditContents (uiEditor st))
  if T.null editorContents
    then do
      liftIO $ atomically $ writeTBQueue (cmdQueue (uiChannels st)) CmdQuit
      halt
    else modify $ \s -> s { uiEditor = E.applyEdit Z.deleteChar (uiEditor s), uiCompletion = Nothing }

-- ^Z: Background process to shell (SIGTSTP) and restore terminal on fg
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'z') [V.MCtrl])) = do
  st <- get
  suspendAndResume $ do
    raiseSignal sigTSTP
    pure st

-- Esc: If completion active -> dismiss completion
--      If HUD active -> dismiss HUD
--      If viewing a subagent -> return to main chat
--      If on main chat -> double Esc interrupts active turn
handleAppEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  st <- get
  case uiCompletion st of
    Just _ -> put st { uiCompletion = Nothing }
    Nothing ->
      if uiShowHud st
        then put st { uiShowHud = False }
        else case uiSelectedSubAgent st of
          Just _ -> put st { uiSelectedSubAgent = Nothing }
          Nothing -> do
            now <- liftIO getCurrentTime
            case uiLastEscTime st of
              Just prev | diffUTCTime now prev < 0.5 -> do
                put st { uiLastEscTime = Nothing }
                liftIO $ atomically $ do
                  writeTBQueue (cmdQueue (uiChannels st)) CmdInterrupt
                  writeTBQueue (cmdQueue (uiChannels st)) (CmdSystemMessage "⚠️ Interrupt dispatched (Esc Esc)...")
              _ ->
                put st { uiLastEscTime = Just now }

-- Alt+Left / Meta+Left: Step back in subagents or return to Main Conversation
handleAppEvent (VtyEvent (V.EvKey V.KLeft mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods = do
      st <- get
      let subIds = Map.keys (uiSubAgents st)
      case uiSelectedSubAgent st of
        Nothing -> pure ()
        Just currId ->
          case break (== currId) subIds of
            ([], _) -> put st { uiSelectedSubAgent = Nothing }
            (prevs, _) -> put st { uiSelectedSubAgent = Just (last prevs) }

-- Alt+Right / Meta+Right: Step forward into next subagent
handleAppEvent (VtyEvent (V.EvKey V.KRight mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods = do
      st <- get
      let subIds = Map.keys (uiSubAgents st)
      unless (null subIds) $ do
        case uiSelectedSubAgent st of
          Nothing -> put st { uiSelectedSubAgent = Just (head subIds) }
          Just currId ->
            case dropWhile (/= currId) subIds of
              (_ : nextId : _) -> put st { uiSelectedSubAgent = Just nextId }
              _                -> pure ()

-- Bracketed Paste: Paste clipboard directly into editor without line splitting
handleAppEvent (VtyEvent (V.EvPaste bs)) = do
  let pasted = TE.decodeUtf8Lenient bs
  modify $ \s -> s { uiEditor = E.applyEdit (Z.insertMany pasted) (uiEditor s) }

-- =========================================================================
-- Readline Editing Conventions in Input Bar
-- =========================================================================

-- ^W or ^Backspace or ^H: Delete preceding word
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit deleteWordBackward (uiEditor s), uiCompletion = Nothing }
handleAppEvent (VtyEvent (V.EvKey V.KBS [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit deleteWordBackward (uiEditor s), uiCompletion = Nothing }
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit deleteWordBackward (uiEditor s), uiCompletion = Nothing }

-- ^U: Kill line backwards from cursor
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit Z.killToBOL (uiEditor s), uiCompletion = Nothing }

-- ^K: Kill line forward to end of line
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit Z.killToEOL (uiEditor s), uiCompletion = Nothing }

-- Alt+H / Meta+H, F1, or Alt+P: Toggle Intelligence HUD
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'h') mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods =
      modify $ \s -> s { uiShowHud = not (uiShowHud s) }

handleAppEvent (VtyEvent (V.EvKey (V.KFun 1) [])) =
  modify $ \s -> s { uiShowHud = not (uiShowHud s) }

handleAppEvent (VtyEvent (V.EvKey (V.KChar 'p') mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods =
      modify $ \s -> s { uiShowHud = not (uiShowHud s) }

-- Alt+M / Meta+M or F2: Toggle Plan / Exec Mode
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'm') mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods = toggleMode

handleAppEvent (VtyEvent (V.EvKey (V.KFun 2) [])) = toggleMode

-- Alt+B: Move backward one word
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'b') mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods =
      modify $ \s -> s { uiEditor = E.applyEdit moveWordBackward (uiEditor s), uiCompletion = Nothing }

-- Alt+F: Move forward one word
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'f') mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods =
      modify $ \s -> s { uiEditor = E.applyEdit moveWordForward (uiEditor s), uiCompletion = Nothing }

-- Alt+D: Delete word forward
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'd') mods))
  | any (`elem` [V.MMeta, V.MAlt]) mods =
      modify $ \s -> s { uiEditor = E.applyEdit deleteWordForward (uiEditor s), uiCompletion = Nothing }

-- Tab: Contextual autocomplete
handleAppEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  st <- get
  case uiCompletion st of
    -- If candidates already active, cycle forward in-place
    Just (CompletionState cands sel) | not (null cands) -> do
      let nextSel = (sel + 1) `mod` length cands
          selected = cands !! nextSel
      put st
        { uiEditor     = replaceCurrentToken (candInsert selected) (uiEditor st)
        , uiCompletion = Just (CompletionState cands nextSel)
        }
    -- Not yet active, trigger contextual completion
    Nothing -> do
      let rawLines = E.getEditContents (uiEditor st)
          rawInput = T.concat rawLines
      mComp <- liftIO $ completeInput "." st rawInput
      case mComp of
        Nothing -> pure ()
        Just (CompletionState [single] _) -> do
          -- Single unambiguous match: auto-fill in-place immediately, zero UI clutter
          put st
            { uiEditor     = replaceCurrentToken (candInsert single) (uiEditor st)
            , uiCompletion = Nothing
            }
        Just cs@(CompletionState (firstMatch:_) _) -> do
          -- Multiple matches: fill first match in-place and show minimal candidate bar
          put st
            { uiEditor     = replaceCurrentToken (candInsert firstMatch) (uiEditor st)
            , uiCompletion = Just cs
            }
        _ -> pure ()
    _ -> pure ()

-- Shift+Tab (BackTab): Cycle completion backward
handleAppEvent (VtyEvent (V.EvKey V.KBackTab [])) = do
  st <- get
  case uiCompletion st of
    Just (CompletionState cands sel) | not (null cands) -> do
      let prevSel = if sel <= 0 then length cands - 1 else sel - 1
          selected = cands !! prevSel
      put st
        { uiEditor     = replaceCurrentToken (candInsert selected) (uiEditor st)
        , uiCompletion = Just (CompletionState cands prevSel)
        }
    _ -> pure ()

-- ^A: Jump to beginning of line
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit Z.gotoBOL (uiEditor s) }

-- ^E: Jump to end of line
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) =
  modify $ \s -> s { uiEditor = E.applyEdit Z.gotoEOL (uiEditor s) }

-- ^T: Toggle thinking accordions
handleAppEvent (VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) = do
  st <- get
  let newVis = not (uiThinkingVisible st)
      targetVis = if newVis then Visible else Collapsed
      updatedSubs = Map.map (\t -> t { subAgentTurns = map (setThinkingVis targetVis) (subAgentTurns t) }) (uiSubAgents st)
  put st
    { uiThinkingVisible = newVis
    , uiTurns = map (setThinkingVis targetVis) (uiTurns st)
    , uiSubAgents = updatedSubs
    }

-- Keystrokes when permission modal is open (Keys 1-4)
handleAppEvent (VtyEvent (V.EvKey (V.KChar '1') [])) = resolveActiveModal PermAlways
handleAppEvent (VtyEvent (V.EvKey (V.KChar '2') [])) = resolveActiveModal PermOnce
handleAppEvent (VtyEvent (V.EvKey (V.KChar '3') [])) = resolveActiveModal PermNo
handleAppEvent (VtyEvent (V.EvKey (V.KChar '4') [])) = resolveActiveModal PermNever

-- Mouse clicks on modal buttons (if mouse events are enabled/passed)
handleAppEvent (MouseDown ButtonAlways V.BLeft _ _) = resolveActiveModal PermAlways
handleAppEvent (MouseDown ButtonOnce   V.BLeft _ _) = resolveActiveModal PermOnce
handleAppEvent (MouseDown ButtonNo     V.BLeft _ _) = resolveActiveModal PermNo
handleAppEvent (MouseDown ButtonNever  V.BLeft _ _) = resolveActiveModal PermNever

-- Mouse click on subagent in sidebar
handleAppEvent (MouseDown (SubAgentItem sId) V.BLeft _ _) =
  modify $ \s -> s { uiSelectedSubAgent = Just sId }

-- Mouse click on thinking accordion fold header
handleAppEvent (MouseDown (ThinkingFold tId) V.BLeft _ _) =
  modify $ \s ->
    let updateTurns = map (toggleThinking tId)
        updateSub t = t { subAgentTurns = updateTurns (subAgentTurns t) }
    in s { uiTurns = updateTurns (uiTurns s)
         , uiSubAgents = Map.map updateSub (uiSubAgents s)
         }
  where
    toggleThinking targetId t@(Turn _ _ blks) =
      t { turnBlocks = map (flipVis targetId) blks }
    flipVis targetId (ThinkingBlock i b vis)
      | i == targetId = ThinkingBlock i b (if vis == Visible then Collapsed else Visible)
    flipVis _ other = other

-- =========================================================================
-- Keyboard-Centric Viewport Scrolling
-- =========================================================================

-- PageUp / PageDown: Scroll conversation viewport in large steps
handleAppEvent (VtyEvent (V.EvKey V.KPageUp _)) =
  vScrollBy (viewportScroll ChatView) (-12)

handleAppEvent (VtyEvent (V.EvKey V.KPageDown _)) =
  vScrollBy (viewportScroll ChatView) 12

-- Up / Down Arrow Keys: Navigate autocomplete if active, else scroll conversation
handleAppEvent (VtyEvent (V.EvKey V.KUp [])) = do
  st <- get
  case uiCompletion st of
    Just (CompletionState cands sel) | not (null cands) -> do
      let prevSel = if sel <= 0 then length cands - 1 else sel - 1
          selected = cands !! prevSel
      put st
        { uiEditor     = replaceCurrentToken (candInsert selected) (uiEditor st)
        , uiCompletion = Just (CompletionState cands prevSel)
        }
    _ -> vScrollBy (viewportScroll ChatView) (-2)

handleAppEvent (VtyEvent (V.EvKey V.KDown [])) = do
  st <- get
  case uiCompletion st of
    Just (CompletionState cands sel) | not (null cands) -> do
      let nextSel = if sel >= length cands - 1 then 0 else sel + 1
          selected = cands !! nextSel
      put st
        { uiEditor     = replaceCurrentToken (candInsert selected) (uiEditor st)
        , uiCompletion = Just (CompletionState cands nextSel)
        }
    _ -> vScrollBy (viewportScroll ChatView) 2

-- Home / End (with Ctrl): Jump to beginning or end of conversation
handleAppEvent (VtyEvent (V.EvKey V.KHome [V.MCtrl])) =
  vScrollToBeginning (viewportScroll ChatView)

handleAppEvent (VtyEvent (V.EvKey V.KEnd [V.MCtrl])) =
  vScrollToEnd (viewportScroll ChatView)

-- =========================================================================
-- Prompt History Navigation (^P = Previous, ^N = Next)
-- =========================================================================

-- ^P: Previous prompt in history (older)
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) = do
  st <- get
  let hist = uiPromptHistory st
  if null hist
    then pure ()
    else case uiHistoryIndex st of
      Nothing -> do
        let currentDraft = T.strip (T.unlines (E.getEditContents (uiEditor st)))
            firstPrompt = head hist
        put st
          { uiSavedDraft   = currentDraft
          , uiHistoryIndex = Just 0
          , uiEditor       = setEditorText firstPrompt
          }
      Just idx -> do
        let nextIdx = min (length hist - 1) (idx + 1)
            recalled = hist !! nextIdx
        put st
          { uiHistoryIndex = Just nextIdx
          , uiEditor       = setEditorText recalled
          }

-- ^N: Next prompt in history (newer / back to draft)
handleAppEvent (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) = do
  st <- get
  let hist = uiPromptHistory st
  case uiHistoryIndex st of
    Nothing -> pure ()
    Just 0  -> do
      let draft = uiSavedDraft st
      put st
        { uiHistoryIndex = Nothing
        , uiSavedDraft   = ""
        , uiEditor       = setEditorText draft
        }
    Just idx -> do
      let prevIdx = idx - 1
          recalled = hist !! prevIdx
      put st
        { uiHistoryIndex = Just prevIdx
        , uiEditor       = setEditorText recalled
        }

-- Keyboard Enter: Submit prompt or dispatch slash command
handleAppEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- get
  case uiCompletion st of
    Just (CompletionState cands sel) | not (null cands) && sel >= 0 && sel < length cands -> do
      let selected = cands !! sel
          fullCmd = T.strip (T.unlines (E.getEditContents (replaceCurrentToken (candInsert selected) (uiEditor st))))
      put st
        { uiEditor        = E.editor EditorInput (Just 1) ""
        , uiCompletion    = Nothing
        , uiHistoryIndex  = Nothing
        , uiSavedDraft    = ""
        }
      vScrollToEnd (viewportScroll ChatView)
      handleCommand fullCmd
    _ -> do
      let rawLines = E.getEditContents (uiEditor st)
          inputText = T.strip (T.unlines rawLines)
      if T.null inputText
        then pure ()
        else do
          -- Record prompt in history (avoiding consecutive duplicates)
          let currentHist = uiPromptHistory st
              newHist = case currentHist of
                (p:_) | p == inputText -> currentHist
                _                      -> inputText : currentHist
          -- Reset input editor and history navigation state
          put st
            { uiEditor        = E.editor EditorInput (Just 1) ""
            , uiPromptHistory = newHist
            , uiHistoryIndex  = Nothing
            , uiSavedDraft    = ""
            , uiCompletion    = Nothing
            }
          vScrollToEnd (viewportScroll ChatView)
          handleCommand inputText

-- Default text editor input (regular typing)
handleAppEvent (VtyEvent ev) = do
  zoom uiEditorLens (E.handleEditorEvent (VtyEvent ev))
  modify $ \s -> if isJust (uiCompletion s) then s { uiCompletion = Nothing } else s

handleAppEvent _ = pure ()

computeCommandMatches :: Text -> [Text]
computeCommandMatches rawInput = map candInsert (computeCommandCandidates rawInput)

handleCommand :: Text -> EventM ResourceName UIState ()
handleCommand cmdText = do
  st <- get
  let channels = uiChannels st
  case cmdText of
    "/help" -> do
      let helpText = T.unlines
            [ "lambdA Commands & Navigation Reference:"
            , ""
            , "Core Slash Commands:"
            , "  /plan          - Switch to Plan mode (read-only inspection, destructive tools disabled)"
            , "  /exec          - Switch to Exec mode (full tool execution access)"
            , "  /mode [mode]   - Inspect or switch active mode (/mode plan, /mode exec)"
            , "  /think         - Toggle reasoning / thinking block visibility"
            , "  /session       - List recent sessions (/session <id> to switch, /session prune <N>)"
            , "  /sub <id>      - Inspect SubAgent dialogue and thought trace (/sub main to return)"
            , "  /trace         - Export current session to formatted Markdown artifact"
            , "  /compact       - Trigger manual context compaction of earlier conversation turns"
            , "  /clear         - Clear conversation turns in current session"
            , "  /new           - Start a clean session and persist previous session"
            , "  /help          - Show this command reference"
            , "  /quit          - Exit lambdA safely"
            , ""
            , "Specialist Subagents:"
            , "  surveyor       - Large-scale directory maps, AST survey, symbol extraction"
            , "  debugger       - Failure analysis, sanitizer traces, compiler error triage"
            , "  profiler       - Performance diagnostics (Valgrind, perf, flamegraphs, benchmarks)"
            , "  implementer    - Code refactoring, test cascades, localized edits"
            , "  reviewer       - Adversarial critique, correctness audits, diff inspection"
            , ""
            , "Shortcuts & Terminal Conventions:"
            , "  Ctrl+C         - Cancel draft line / Interrupt active generation"
            , "  Ctrl+D         - Exit lambdA (on empty line) / forward delete character"
            , "  Ctrl+Z         - Suspend/background process to shell (fg to resume)"
            , "  Esc Esc        - Interrupt active turn / cancel pending tools"
            , "  Alt+M / F2     - Toggle Plan mode (read-only) / Exec mode"
            , "  Alt+H / F1     - Toggle Intelligence HUD"
            , "  Tab            - Contextual autocomplete (commands, sessions, subagents, paths)"
            , "  Alt+← / Alt+→  - Navigate between Main Chat and SubAgents"
            , "  Ctrl+W, Ctrl+H - Delete word backward"
            , "  Alt+D          - Delete word forward"
            , "  Alt+B, Alt+F   - Move cursor backward / forward by word"
            , "  Ctrl+U, Ctrl+K - Delete line to start / end"
            , "  Ctrl+A, Ctrl+E - Move cursor to start / end of line"
            , "  Ctrl+T         - Toggle thinking/reasoning blocks"
            , "  Keys 1,2,3,4   - Resolve authorization prompt (Always, Once, No, Never)"
            , "  Mouse Click    - Click on SubAgent #id in sidebar or thinking folds"
            ]
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSystemMessage helpText)
    "/new" -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdNewSession
    cmd | cmd `elem` ["/session", "/sessions"] -> do
      metas <- liftIO $ listSessions ".lambda/sessions"
      let total = length metas
          recent = take 20 metas
          header = "Recent Sessions (" <> T.pack (show (length recent)) <> " of " <> T.pack (show total) <> "):\n"
          formatMeta m =
            let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (metaUpdatedAt m)
                turnsStr = T.pack $ show (metaTurnCount m)
                agentsStr = T.pack $ show (metaSubAgentCount m)
            in "  " <> metaId m <> " | " <> timeStr <> " | " <> turnsStr <> " turns | " <> agentsStr <> " subagents | " <> metaTitle m
          msg = if null metas
                  then "No saved sessions found in .lambda/sessions/"
                  else header <> T.unlines (map formatMeta recent) <> "\nUse '/session <id>' to switch, or '/session prune <keep>' to clean up."
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSystemMessage msg)
    cmd | "/session prune " `T.isPrefixOf` cmd -> do
      let arg = T.strip (T.drop 15 cmd)
      case reads (T.unpack arg) of
        [(n, "")] -> do
          pruned <- liftIO $ pruneSessions ".lambda/sessions" n
          liftIO $ atomically $ writeTBQueue (cmdQueue channels)
            (CmdSystemMessage $ "Pruned " <> T.pack (show pruned) <> " old session(s). Keeping " <> T.pack (show (n :: Int)) <> " most recent.")
        _ ->
          liftIO $ atomically $ writeTBQueue (cmdQueue channels)
            (CmdSystemMessage "Usage: /session prune <keep_count> (e.g. /session prune 20)")
    cmd | "/session " `T.isPrefixOf` cmd -> do
      let targetId = T.strip (T.drop 9 cmd)
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSwitchSession targetId)
    cmd | cmd `elem` ["/trace", "/export", "/dump"] -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdExportTrace
    cmd | cmd `elem` ["/mode plan", "/plan"] -> do
      put st { uiMode = PlanMode }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSetMode PlanMode)
    cmd | cmd `elem` ["/mode exec", "/exec"] -> do
      put st { uiMode = ExecMode }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) (CmdSetMode ExecMode)
    "/mode" -> do
      let modeStr = if uiMode st == PlanMode then "Plan (read-only)" else "Exec (full access)"
      liftIO $ atomically $ writeTBQueue (cmdQueue channels)
        (CmdSystemMessage $ "Current mode: " <> modeStr <> "\nUse '/mode plan' or '/mode exec' (or press Alt+M) to switch.")
    cmd | cmd `elem` ["/think", "/thinking"] -> do
      let newVis = not (uiThinkingVisible st)
          targetVis = if newVis then Visible else Collapsed
          statusMsg = if newVis then "expanded" else "collapsed"
          updatedSubs = Map.map (\t -> t { subAgentTurns = map (setThinkingVis targetVis) (subAgentTurns t) }) (uiSubAgents st)
      put st
        { uiThinkingVisible = newVis
        , uiTurns = map (setThinkingVis targetVis) (uiTurns st)
        , uiSubAgents = updatedSubs
        }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels)
        (CmdSystemMessage $ "Thinking scratchpad: " <> statusMsg)
    cmd | cmd `elem` ["/think on", "/think show", "/thinking on", "/thinking show"] -> do
      let updatedSubs = Map.map (\t -> t { subAgentTurns = map (setThinkingVis Visible) (subAgentTurns t) }) (uiSubAgents st)
      put st
        { uiThinkingVisible = True
        , uiTurns = map (setThinkingVis Visible) (uiTurns st)
        , uiSubAgents = updatedSubs
        }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels)
        (CmdSystemMessage "Thinking scratchpad: expanded")
    cmd | cmd `elem` ["/think off", "/think hide", "/thinking off", "/thinking hide"] -> do
      let updatedSubs = Map.map (\t -> t { subAgentTurns = map (setThinkingVis Collapsed) (subAgentTurns t) }) (uiSubAgents st)
      put st
        { uiThinkingVisible = False
        , uiTurns = map (setThinkingVis Collapsed) (uiTurns st)
        , uiSubAgents = updatedSubs
        }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels)
        (CmdSystemMessage "Thinking scratchpad: collapsed")
    cmd | "/sub " `T.isPrefixOf` cmd || "/subagent " `T.isPrefixOf` cmd -> do
      let arg = T.strip $ if "/sub " `T.isPrefixOf` cmd then T.drop 5 cmd else T.drop 10 cmd
      if arg `elem` ["main", "back", "chat", "0", "exit"]
        then do
          put st { uiSelectedSubAgent = Nothing }
          liftIO $ atomically $ writeTBQueue (cmdQueue channels)
            (CmdSystemMessage "Switched to Main Conversation.")
        else
          case reads (T.unpack arg) of
            [(sId, "")] ->
              if Map.member sId (uiSubAgents st)
                then do
                  put st { uiSelectedSubAgent = Just sId }
                  liftIO $ atomically $ writeTBQueue (cmdQueue channels)
                    (CmdSystemMessage $ "Viewing SubAgent #" <> T.pack (show sId) <> " Dialogue & CoT. Press Esc or Alt+← to return.")
                else
                  liftIO $ atomically $ writeTBQueue (cmdQueue channels)
                    (CmdSystemMessage $ "⚠️ SubAgent #" <> arg <> " not found.")
            _ ->
              liftIO $ atomically $ writeTBQueue (cmdQueue channels)
                (CmdSystemMessage "Usage: /sub <id> to view subagent CoT, or /sub main to return.")
    "/compact" -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdCompactHistory
    "/clear" -> do
      put st { uiTurns = [] }
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdClearHistory
    "/quit" -> do
      liftIO $ atomically $ writeTBQueue (cmdQueue channels) CmdQuit
      halt
    _ -> do
      put st { uiIsGenerating = True }
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

-- | Deletes backwards by word respecting whitespace boundaries (like Readline ^W)
deleteWordBackward :: Z.TextZipper Text -> Z.TextZipper Text
deleteWordBackward z =
  let (_, col) = Z.cursorPosition z
      line = Z.currentLine z
      before = T.take col line
  in if T.null before
       then Z.deletePrevChar z
       else
         let trimmed = T.dropWhileEnd isSpace before
             keep    = T.dropWhileEnd (not . isSpace) trimmed
             count   = T.length before - T.length keep
         in iterate Z.deletePrevChar z !! max 1 count

-- | Moves cursor backward by word respecting whitespace boundaries (like Readline Alt+B)
moveWordBackward :: Z.TextZipper Text -> Z.TextZipper Text
moveWordBackward z =
  let (_, col) = Z.cursorPosition z
      line = Z.currentLine z
      before = T.take col line
  in if T.null before
       then Z.moveLeft z
       else
         let trimmed = T.dropWhileEnd isSpace before
             keep    = T.dropWhileEnd (not . isSpace) trimmed
             count   = T.length before - T.length keep
         in iterate Z.moveLeft z !! max 1 count

-- | Moves cursor forward by word respecting whitespace boundaries (like Readline Alt+F)
moveWordForward :: Z.TextZipper Text -> Z.TextZipper Text
moveWordForward z =
  let (_, col) = Z.cursorPosition z
      line = Z.currentLine z
      after = T.drop col line
  in if T.null after
       then Z.moveRight z
       else
         let trimmed = T.dropWhile isSpace after
             keep    = T.dropWhile (not . isSpace) trimmed
             count   = T.length after - T.length keep
         in iterate Z.moveRight z !! max 1 count

-- | Deletes forward by word respecting whitespace boundaries (like Readline Alt+D)
deleteWordForward :: Z.TextZipper Text -> Z.TextZipper Text
deleteWordForward z =
  let (_, col) = Z.cursorPosition z
      line = Z.currentLine z
      after = T.drop col line
  in if T.null after
       then Z.deleteChar z
       else
         let trimmed = T.dropWhile isSpace after
             keep    = T.dropWhile (not . isSpace) trimmed
             count   = T.length after - T.length keep
         in iterate Z.deleteChar z !! max 1 count

-- Lens helper for editor zooming
uiEditorLens :: Functor f => (E.Editor Text ResourceName -> f (E.Editor Text ResourceName)) -> UIState -> f UIState
uiEditorLens f s = (\e -> s { uiEditor = e }) <$> f (uiEditor s)

-- | Helper to populate editor content and place cursor at end of line
setEditorText :: Text -> E.Editor Text ResourceName
setEditorText strVal = E.applyEdit Z.gotoEOL (E.editor EditorInput (Just 1) strVal)

-- | Explicitly sets visibility of all thinking blocks across turns
setThinkingVis :: BlockVisibility -> Turn -> Turn
setThinkingVis targetVis t@(Turn _ _ blks) = t { turnBlocks = map setVis blks }
  where
    setVis (ThinkingBlock i b _) = ThinkingBlock i b targetVis
    setVis other = other

-- | Toggles between PlanMode and ExecMode
toggleMode :: EventM ResourceName UIState ()
toggleMode = do
  st <- get
  let nextMode = if uiMode st == PlanMode then ExecMode else PlanMode
  put st { uiMode = nextMode }
  liftIO $ atomically $ writeTBQueue (cmdQueue (uiChannels st)) (CmdSetMode nextMode)

-- | Replaces current word/token under cursor with completed text
replaceCurrentToken :: Text -> E.Editor Text ResourceName -> E.Editor Text ResourceName
replaceCurrentToken inserted ed =
  let fullText = T.concat (E.getEditContents ed)
      tokens = T.words fullText
  in case tokens of
       [] -> setEditorText (inserted <> if "/" `T.isSuffixOf` inserted then "" else " ")
       _  ->
         let hasTrailingSpace = T.isSuffixOf " " fullText
         in if hasTrailingSpace
              then setEditorText (fullText <> inserted <> if "/" `T.isSuffixOf` inserted then "" else " ")
              else
                let prefixTokens = init tokens
                    prefixStr = if null prefixTokens then "" else T.unwords prefixTokens <> " "
                    newText = prefixStr <> inserted <> if "/" `T.isSuffixOf` inserted then "" else " "
                in setEditorText newText
