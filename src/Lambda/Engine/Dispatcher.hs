{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Dispatcher
  ( executeToolDispatch
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath (isAbsolute, normalise, pathSeparator, takeDirectory, takeFileName, (</>))
import System.FilePath.Glob (compile, match)

import Lambda.Config (Config(..))
import Lambda.Core.ToolProvider
import Lambda.Engine.Security (checkAuthorization)
import Lambda.Engine.State (AppEngineState(..))
import Lambda.Types

-- | Verifies whether a given target path resolves outside the workspace directory
isPathOutsideWorkspace :: FilePath -> FilePath -> IO Bool
isPathOutsideWorkspace wsRoot rawPath = do
  canonicalRoot <- canonicalizePath wsRoot
  let candidate = if isAbsolute rawPath then rawPath else wsRoot </> rawPath
  exists <- doesPathExist candidate
  canonicalTarget <- if exists
    then canonicalizePath candidate
    else do
      let parent = takeDirectory candidate
      pExists <- doesPathExist parent
      canParent <- if pExists
        then canonicalizePath parent
        else pure (normalise parent)
      pure (normalise (canParent </> takeFileName candidate))
  let cleanRoot = normalise canonicalRoot
      cleanTarget = normalise canonicalTarget
      isContained = cleanTarget == cleanRoot
                 || (cleanRoot ++ [pathSeparator]) `isPrefixOf` cleanTarget
  pure (not isContained)

-- | Dispatches tool calls with strict mode enforcement, workspace confinement, and JIT capability security
executeToolDispatch
  :: AppEngineState
  -> CallerContext
  -> [Text]              -- Pre-authorized capability grants (for SubAgents)
  -> ToolCall
  -> IO ToolResult
executeToolDispatch AppEngineState{..} caller grantedGlobs ToolCall{..} = do
  (mode, reg) <- atomically $ do
    m <- readTVar appMode
    r <- readTVar appToolRegistry
    pure (m, r)

  case lookupTool toolCallName reg of
    Nothing ->
      pure $ ToolResult toolCallId "" ("Tool not found in registry: " <> toolCallName) Nothing
    Just ToolDefinition{..} -> do
      -- Step 1: Mode Safety Invariant (/plan vs /exec)
      if mode == PlanMode && toolCapability == Destructive
        then pure $ ToolResult toolCallId ""
               "Execution rejected: Mutating / destructive operations are disabled in [/plan] mode. Switch to [/exec] to run."
               Nothing
        else do
          -- Extract tool arguments for JIT parameter inspection
          let mCommand = case parseEither (Aeson.withObject "args" (Aeson..: "command")) toolCallArgs of
                Right (cmd :: Text) -> Just cmd
                _                   -> Nothing
              mPath = case parseEither (Aeson.withObject "args" (Aeson..: "path")) toolCallArgs of
                Right (p :: Text) -> Just (T.unpack p)
                _                 -> Nothing
              isWebFetch = toolCallName == "fetch_url"

          -- Step 2: Workspace confinement check
          isOutsidePath <- case mPath of
            Just p  -> isPathOutsideWorkspace (workspaceRoot appConfig) p
            Nothing -> pure False

          -- Step 3: Classify if command is destructive or modifying
          let isSafeBashCommand cmd =
                let stripped = T.stripStart cmd
                in any (`T.isPrefixOf` stripped)
                     [ "git status", "git diff", "git log", "ls", "pwd", "cat "
                     , "perf ", "valgrind ", "cabal test", "ctest", "pytest"
                     ]
              isDestructiveCommand =
                case mCommand of
                  Just cmd -> not (isSafeBashCommand cmd)
                  Nothing  -> toolCapability == Destructive

          let requiresExplicitPermission = isOutsidePath || isWebFetch || isDestructiveCommand

          -- Construct display command representation for authorization prompt
          let displayCmd = case mCommand of
                Just cmd -> cmd
                Nothing  -> case mPath of
                  Just p | isOutsidePath -> "outside_workspace: " <> toolCallName <> " " <> T.pack p
                         | otherwise     -> toolCallName <> " " <> T.pack p
                  Nothing                -> toolCallName

          -- Step 4: Capability & Security Authorization
          authorized <- case caller of
            SubAgentId _ _ -> do
              let cmdStr = case mCommand of
                    Just cmd -> T.unpack cmd
                    Nothing  -> T.unpack toolCallName
                  matchesGrant = any (`matchesPattern` cmdStr) grantedGlobs

              if not matchesGrant
                then
                  -- Ungranted tool -> dynamic permission escalation
                  checkAuthorization appSecurity caller displayCmd toolCallArgs
                else if requiresExplicitPermission
                  then
                    -- Granted capability, but sensitive operation (outside path, destructive, or webfetch)
                    -- Check session memory or prompt user JIT
                    checkAuthorization appSecurity caller displayCmd toolCallArgs
                  else
                    -- Safe local read-only workspace operation matching subagent grants -> auto-allow
                    pure True

            MainAgent -> do
              if toolCallName == "spawn_specialist_subagent" || (toolCapability == ReadOnly && not requiresExplicitPermission)
                then pure True
                else checkAuthorization appSecurity caller displayCmd toolCallArgs

          if not authorized
            then pure $ ToolResult toolCallId ""
                   ("Permission Denied: Operation not authorized for " <> T.pack (show caller))
                   Nothing
            else do
              -- Step 5: Execute tool with exception guard
              execRes <- try (toolExecute caller toolCallArgs)
              case execRes of
                Left (ex :: SomeException) ->
                  pure $ ToolResult toolCallId "" ("Tool execution failure: " <> T.pack (show ex)) Nothing
                Right res ->
                  pure res { resultCallIdRef = toolCallId }
  where
    matchesPattern globPat str =
      let patStr = T.unpack globPat
          starPat = if "*" `T.isSuffixOf` globPat && not ("**" `T.isSuffixOf` globPat)
                      then T.unpack (globPat <> "*")
                      else patStr
      in match (compile patStr) str
         || match (compile starPat) str
         || (if "*" `T.isSuffixOf` globPat
               then T.dropEnd 1 globPat `T.isPrefixOf` T.pack str
               else globPat == T.pack str)
