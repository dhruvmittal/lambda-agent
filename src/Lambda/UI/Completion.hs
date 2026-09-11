{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Completion
  ( allCommands
  , completeInput
  , computeCommandCandidates
  , slidingCandidateWindow
  , Candidate(..)
  ) where

import Control.Exception (catch, SomeException)
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), splitFileName)

import Lambda.Config (Config(..), isLocalEndpoint, isOpenAiEndpoint, isOpenRouterEndpoint)
import Lambda.Engine.PromptMacro (listPromptMacros)
import Lambda.Engine.Session (listSessions, SessionMeta(..))
import Lambda.Types
import Lambda.UI.Types

-- | Canonical list of supported slash commands
allCommands :: [Text]
allCommands =
  [ "/plan"
  , "/exec"
  , "/mode"
  , "/model"
  , "/models"
  , "/think"
  , "/session"
  , "/fork"
  , "/rewind"
  , "/undo"
  , "/prompt"
  , "/p"
  , "/sub"
  , "/trace"
  , "/compact"
  , "/clear"
  , "/new"
  , "/help"
  , "/quit"
  ]

-- | Compute slash command candidates matching a given prefix
computeCommandCandidates :: Text -> [Candidate]
computeCommandCandidates rawInput =
  let clean = T.dropWhile (== ' ') (T.filter (\c -> c /= '\n' && c /= '\r') rawInput)
  in if "/" `T.isPrefixOf` clean && not (" " `T.isInfixOf` clean)
       then [ Candidate cmd cmd | cmd <- allCommands, clean `T.isPrefixOf` cmd ]
       else []

-- | Core contextual completion dispatcher
completeInput :: FilePath -> UIState -> Text -> IO (Maybe CompletionState)
completeInput baseDir st rawInput = do
  let clean = T.dropWhile (== ' ') (T.filter (\c -> c /= '\n' && c /= '\r') rawInput)
  case () of
    -- 1. Slash command name completion (e.g. "/p" -> "/plan")
    _ | "/" `T.isPrefixOf` clean && not (" " `T.isInfixOf` clean) -> do
        let cands = computeCommandCandidates clean
        pure $ toCompletionState cands

    -- 2. /session argument completion (MRU sorted)
    _ | "/session " `T.isPrefixOf` clean -> do
        let sessArg = T.drop 9 clean
            sessDir = baseDir </> ".lambda" </> "sessions"
        exists <- doesDirectoryExist sessDir
        if not exists
          then pure Nothing
          else do
            metas <- catch (listSessions sessDir) (\(_ :: SomeException) -> pure [])
            let sorted = sortOn (Down . metaUpdatedAt) metas
                formatted = [ Candidate (metaId m) (formatSessionMeta m)
                            | m <- sorted
                            , sessArg `T.isPrefixOf` metaId m || T.null sessArg
                            ]
                extraPrune = if sessArg `T.isPrefixOf` "prune" && not (T.null sessArg)
                               then [Candidate "prune " "prune <keep_count>"]
                               else []
            pure $ toCompletionState (extraPrune ++ formatted)

    -- 3. /sub argument completion (active subagent IDs + main)
    _ | "/sub " `T.isPrefixOf` clean -> do
        let subArg = T.drop 5 clean
            subTasks = Map.elems (uiSubAgents st)
            activeSubs =
              [ Candidate (T.pack (show subAgentId)) ("#" <> T.pack (show subAgentId) <> " [" <> T.toUpper subAgentRole <> "] (" <> formatStatus subAgentStatus <> ")")
              | SubAgentTask{..} <- subTasks
              , subArg `T.isPrefixOf` T.pack (show subAgentId) || T.null subArg
              ]
            mainOpt = if subArg `T.isPrefixOf` "main"
                        then [Candidate "main" "main (dialogue)"]
                        else []
        pure $ toCompletionState (activeSubs ++ mainOpt)

    -- 4. /mode argument completion
    _ | "/mode " `T.isPrefixOf` clean -> do
        let modeArg = T.drop 6 clean
            allModes =
              [ Candidate "plan" "plan (read-only safe)"
              , Candidate "exec" "exec (full access)"
              ]
            matched = [ c | c <- allModes, modeArg `T.isPrefixOf` candInsert c || T.null modeArg ]
        pure $ toCompletionState matched

    -- 5. /model and /models argument completion
    _ | "/model " `T.isPrefixOf` clean || "/models " `T.isPrefixOf` clean -> do
        let modArg = if "/models " `T.isPrefixOf` clean then T.drop 8 clean else T.drop 7 clean
            cfg = uiConfig st
            baseUrl = apiBaseUrl cfg
            userAliases = modelAliases cfg
            userModels = configuredModels cfg
            activeMod = uiModelName st

            aliasCands = [ Candidate a (a <> " (" <> target <> ")") | (a, target) <- Map.toList userAliases ]
            userCands = [ Candidate m m | m <- userModels ]
            activeCand = if T.null activeMod then [] else [ Candidate activeMod (activeMod <> " (active)") ]

            endpointCands
              | T.null baseUrl = [] -- STRICT INVARIANT: If unconfigured, NEVER guess or suggest cloud models!
              | isLocalEndpoint baseUrl = [] -- STRICT INVARIANT: Never show cloud models on local endpoints!
              | isOpenAiEndpoint baseUrl =
                  [ Candidate "4o"      "4o (gpt-4o, 128k)"
                  , Candidate "4o-mini" "4o-mini (gpt-4o-mini, 128k)"
                  , Candidate "o3"      "o3 (o3-mini, 200k)"
                  , Candidate "o1"      "o1 (o1, 200k)"
                  , Candidate "gpt-4o"  "gpt-4o (128k)"
                  , Candidate "gpt-4o-mini" "gpt-4o-mini (128k)"
                  , Candidate "o3-mini" "o3-mini (200k)"
                  ]
              | isOpenRouterEndpoint baseUrl = -- Only when OpenRouter is explicitly configured
                  [ Candidate "claude"     "claude (anthropic/claude-3.5-sonnet, 200k)"
                  , Candidate "claude-3.7" "claude-3.7 (anthropic/claude-3.7-sonnet, 200k)"
                  , Candidate "r1"         "r1 (deepseek/deepseek-r1, 128k)"
                  , Candidate "4o"         "4o (openai/gpt-4o, 128k)"
                  , Candidate "o3"         "o3 (openai/o3-mini, 200k)"
                  , Candidate "qwen"       "qwen (qwen-2.5-coder-32b, 128k)"
                  , Candidate "anthropic/claude-3.5-sonnet" "anthropic/claude-3.5-sonnet"
                  , Candidate "anthropic/claude-3.7-sonnet" "anthropic/claude-3.7-sonnet"
                  , Candidate "deepseek/deepseek-r1" "deepseek/deepseek-r1"
                  , Candidate "openai/gpt-4o" "openai/gpt-4o"
                  , Candidate "openai/o3-mini" "openai/o3-mini"
                  , Candidate "qwen/qwen-2.5-coder-32b-instruct" "qwen/qwen-2.5-coder-32b-instruct"
                  ]
              | otherwise = [] -- Generic remote: rely exclusively on configured models and aliases

            dedupCandidates = foldr (\c acc -> if any (\x -> candInsert x == candInsert c) acc then acc else c : acc) []
            allCandidates = dedupCandidates (aliasCands ++ userCands ++ activeCand ++ endpointCands)
            matched = [ c | c <- allCandidates, modArg `T.isPrefixOf` candInsert c || T.null modArg ]
        pure $ toCompletionState matched

    -- 5. /think argument completion
    _ | "/think " `T.isPrefixOf` clean -> do
        let thinkArg = T.drop 7 clean
            thinkOpts =
              [ Candidate "toggle" "toggle (switch reasoning visibility)"
              , Candidate "show"   "show (always visible)"
              , Candidate "hide"   "hide (collapsed accordions)"
              ]
            matched = [ c | c <- thinkOpts, thinkArg `T.isPrefixOf` candInsert c || T.null thinkArg ]
        pure $ toCompletionState matched

    -- 6. /prompt and /p argument completion (templates from .lambda/prompts/*.md)
    _ | "/prompt " `T.isPrefixOf` clean || "/p " `T.isPrefixOf` clean -> do
        let pArg = if "/prompt " `T.isPrefixOf` clean then T.drop 8 clean else T.drop 3 clean
        macroNames <- catch (listPromptMacros baseDir) (\(_ :: SomeException) -> pure [])
        let matched = [ Candidate m (".lambda/prompts/" <> m <> ".md")
                      | m <- macroNames
                      , pArg `T.isPrefixOf` m || T.null pArg
                      ]
        pure $ toCompletionState matched

    -- 7. Filesystem path completion (words starting with '@' or containing '/')
    _ -> do
        let lastToken = case T.words clean of
              [] -> ""
              ws -> last ws
        if "@" `T.isPrefixOf` lastToken || "/" `T.isInfixOf` lastToken || "./" `T.isPrefixOf` lastToken
          then do
            let hasAt = "@" `T.isPrefixOf` lastToken
                rawPath = if hasAt then T.drop 1 lastToken else lastToken
                pathStr = T.unpack rawPath
                (dirPart, filePart) = splitFileName pathStr
                searchDir = if null dirPart then baseDir else baseDir </> dirPart
            dirExists <- doesDirectoryExist searchDir
            if not dirExists
              then pure Nothing
              else do
                entries <- catch (listDirectory searchDir) (\(_ :: SomeException) -> pure [])
                let filtered = filter isAllowedPath entries
                    matches = filter (filePart `isPrefixOf`) filtered
                cands <- mapM (formatPathCandidate baseDir dirPart hasAt) (take 25 matches)
                pure $ toCompletionState cands
          else pure Nothing
  where
    toCompletionState [] = Nothing
    toCompletionState cs = Just (CompletionState cs 0)

    formatSessionMeta m =
      let timeStr = T.pack $ formatTime defaultTimeLocale "%H:%M" (metaUpdatedAt m)
          turnsStr = T.pack (show (metaTurnCount m)) <> "t"
          forkStr = case metaParentId m of
            Just _  -> " ↳fork"
            Nothing -> ""
          titleStr = if T.null (metaTitle m) then "" else " · " <> T.take 22 (metaTitle m)
      in metaId m <> " (" <> timeStr <> ", " <> turnsStr <> forkStr <> titleStr <> ")"

    formatStatus SubAgentRunning     = "running"
    formatStatus (SubAgentSuccess _) = "success"
    formatStatus (SubAgentBlocked _) = "blocked"
    formatStatus (SubAgentFailed _)  = "failed"

    isAllowedPath name =
      not (name `elem` [".git", "node_modules", "dist-newstyle", ".lambda", ".stack-work", ".venv", "__pycache__"])
      && (not ("." `isPrefixOf` name) || name == ".lambda" || name == "./")

    formatPathCandidate root dir hasAt name = do
      let relTarget = if null dir then name else dir </> name
          absTarget = root </> relTarget
      isDir <- doesDirectoryExist absTarget
      let suffixed = if isDir then relTarget ++ "/" else relTarget
          prefix = if hasAt then "@" else ""
          fullInsert = prefix ++ suffixed
          fullDisplay = if isDir then name ++ "/" else name
      pure $ Candidate (T.pack fullInsert) (T.pack fullDisplay)

-- | Compute a sliding window of visible candidates to prevent terminal overflow.
-- Returns: (windowSelectedIdx, visibleCandidates, hiddenRemainingCount)
slidingCandidateWindow :: Int -> [Candidate] -> (Int, [Candidate], Int)
slidingCandidateWindow sel allCands
  | null allCands = (0, [], 0)
  | total <= maxVisible = (sel, allCands, 0)
  | otherwise =
      let half = maxVisible `div` 2
          start = max 0 (min (sel - half) (total - maxVisible))
          visible = take maxVisible (drop start allCands)
          subSel = sel - start
          remaining = total - (start + length visible)
      in (subSel, visible, max 0 remaining)
  where
    total = length allCands
    maxVisible = 5
