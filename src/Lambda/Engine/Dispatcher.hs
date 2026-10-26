{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Lambda.Engine.Dispatcher
  ( executeToolDispatch
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (parseEither, Parser)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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
executeToolDispatch engineState@AppEngineState{..} caller grantedGlobs ToolCall{..}
  | toolCallName == "call_mcp_tool" = do
      case parseEither parseCallMcpArgs toolCallArgs of
        Left err ->
          pure $ ToolResult toolCallId "" ("Failed to parse call_mcp_tool arguments: " <> T.pack err) Nothing
        Right (innerName, innerArgs)
          | innerName == "call_mcp_tool" ->
              pure $ ToolResult toolCallId "" "Recursive call_mcp_tool invocation is not permitted." Nothing
          | otherwise -> do
              reg <- readTVarIO appToolRegistry
              case lookupTool innerName reg of
                Nothing ->
                  pure $ ToolResult toolCallId "" ("MCP tool not found in registry: " <> innerName) Nothing
                Just _ -> do
                  let innerCall = ToolCall
                        { toolCallId   = toolCallId
                        , toolCallName = innerName
                        , toolCallArgs = innerArgs
                        }
                  executeToolDispatch engineState caller grantedGlobs innerCall
  | otherwise = do
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
                  isBashCall = toolCallName == "bash"

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

                  -- Command-level grant match (e.g. "cmake *", "nix build*")
                  matchesCmdGrant = case mCommand of
                    Just cmd -> any (`matchesPattern` T.unpack cmd) grantedGlobs
                    Nothing  -> False

                  -- Tool-level generic grant match (e.g. "bash*", "read_file*")
                  hasGenericToolGrant = any (`matchesPattern` T.unpack toolCallName) grantedGlobs
                                     || (isBashCall && any (`matchesPattern` "bash") grantedGlobs)

                  matchesGrant = hasGenericToolGrant || matchesCmdGrant

                  -- Explicit command-specific grants (e.g. "cmake *") pre-authorize execution
                  -- without prompting the interactive ledger unless breaching workspace boundaries
                  requiresExplicitPermission =
                    isOutsidePath || isWebFetch || (isDestructiveCommand && not matchesCmdGrant)

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

parseCallMcpArgs :: Aeson.Value -> Parser (Text, Aeson.Value)
parseCallMcpArgs = Aeson.withObject "call_mcp_tool args" $ \o -> do
  name <- o .: "name"
  mArgsVal <- o .:? "arguments"
  let parsedArgs = case mArgsVal of
        Just (Aeson.Object innerObj) -> Aeson.Object innerObj
        Just (Aeson.String s) ->
          case Aeson.decodeStrict (TE.encodeUtf8 s) of
            Just (Aeson.Object innerObj) -> Aeson.Object innerObj
            _ -> Aeson.Object mempty
        _ -> Aeson.Object mempty
  pure (name, parsedArgs)
