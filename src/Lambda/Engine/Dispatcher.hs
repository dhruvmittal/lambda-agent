{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Lambda.Engine.Dispatcher
  ( executeToolDispatch
  , executeToolDispatchWithPerms
  ) where

import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))
import Data.Aeson.Types (parseEither, Parser)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath (isAbsolute, normalise, pathSeparator, takeDirectory, takeFileName, (</>))

import Lambda.Config (Config(..))
import Lambda.Core.ToolProvider
import Lambda.Engine.Security (CallerGrantContext(..), SecuritySubject(..), checkAuthorizationSubject)
import Lambda.Engine.State (AppEngineState(..))
import Lambda.Types

-- | Verifies whether a given target path resolves outside the workspace directory
isPathOutsideWorkspace :: FilePath -> FilePath -> IO Bool
isPathOutsideWorkspace wsRoot rawPath = do
  canonicalRoot <- canonicalizePath wsRoot
  let candidate = if isAbsolute rawPath then rawPath else wsRoot </> rawPath
  canonicalTarget <- canonicalizeAncestor candidate
  let cleanRoot = normalise canonicalRoot
      cleanTarget = normalise canonicalTarget
      isContained = cleanTarget == cleanRoot
                 || (cleanRoot ++ [pathSeparator]) `isPrefixOf` cleanTarget
  pure (not isContained)
  where
    canonicalizeAncestor p = do
      exists <- doesPathExist p
      if exists
        then canonicalizePath p
        else do
          let parent = takeDirectory p
          if parent == p
            then pure (normalise p)
            else do
              canParent <- canonicalizeAncestor parent
              pure (normalise (canParent </> takeFileName p))

-- | Dispatches tool calls with strict mode enforcement, workspace confinement, and JIT capability security
executeToolDispatch
  :: AppEngineState
  -> CallerContext
  -> [Text]              -- Pre-authorized capability grants (for SubAgents)
  -> ToolCall
  -> IO ToolResult
executeToolDispatch engineState caller grantedGlobs =
  executeToolDispatchWithPerms engineState caller (SubAgentPermissions (Set.fromList grantedGlobs) Set.empty)

-- | Dispatches tool calls with structured SubAgentPermissions (allow + deny globs)
executeToolDispatchWithPerms
  :: AppEngineState
  -> CallerContext
  -> SubAgentPermissions
  -> ToolCall
  -> IO ToolResult
executeToolDispatchWithPerms engineState@AppEngineState{..} caller subPerms ToolCall{..}
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
                  executeToolDispatchWithPerms engineState caller subPerms innerCall
  | otherwise = do
      (mode, reg) <- atomically $ do
        m <- readTVar appMode
        r <- readTVar appToolRegistry
        pure (m, r)

      case lookupTool toolCallName reg of
        Nothing ->
          pure $ ToolResult toolCallId "" ("Tool not found in registry: " <> toolCallName) Nothing
        Just ToolDefinition{..} -> do
          -- Extract tool arguments for structured subject construction
          let mCommand = case parseEither (Aeson.withObject "args" (Aeson..: "command")) toolCallArgs of
                Right (cmd :: Text) -> Just cmd
                _                   -> Nothing
              mPath = case parseEither (Aeson.withObject "args" (Aeson..: "path")) toolCallArgs of
                Right (p :: Text) -> Just (T.unpack p)
                _                 -> Nothing
              mUrl = case parseEither (Aeson.withObject "args" (Aeson..: "url")) toolCallArgs of
                Right (u :: Text) -> Just u
                _                 -> Nothing

          isOutsidePath <- case mPath of
            Just p  -> isPathOutsideWorkspace (workspaceRoot appConfig) p
            Nothing -> pure False

          let subject = case toolCallName of
                "bash"      -> SubjBash (fromMaybe "" mCommand)
                "fetch_url" -> SubjWebFetch (fromMaybe "" mUrl)
                _           -> SubjTool toolCallName toolCapability mPath isOutsidePath
              grantCtx = CallerGrantContext caller subPerms mode

          authOutcome <- checkAuthorizationSubject appSecurity grantCtx subject toolCallArgs (Just toolCallId)
          case authOutcome of
            Authorized -> do
              execRes <- try (toolExecute caller toolCallArgs)
              case execRes of
                Left (ex :: SomeException) ->
                  pure $ ToolResult toolCallId "" ("Tool execution failure: " <> T.pack (show ex)) Nothing
                Right res ->
                  pure res { resultCallIdRef = toolCallId }
            DeniedByPlanMode _ ->
              pure $ ToolResult toolCallId ""
                "Execution rejected: Mutating / destructive operations are disabled in [/plan] mode. Switch to [/exec] to run."
                Nothing
            DeniedByUser _ disp ->
              pure $ ToolResult toolCallId ""
                ("Blocked by user decision: The user denied permission to execute '" <> disp <> "'. Do not retry this command or attempt to bypass this restriction; adjust your plan or ask the user how they would like to proceed.")
                Nothing
            DeniedByPolicy scope rule ->
              let scopeLbl = if scope == Always then "permanent" else "session"
              in pure $ ToolResult toolCallId ""
                   ("Blocked by " <> scopeLbl <> " policy (" <> rule <> "): Operation not authorized for " <> T.pack (show caller) <> ". Do not retry or bypass this restriction.")
                   Nothing
            DeniedByCancel ->
              pure $ ToolResult toolCallId ""
                "Blocked by user decision: Permission prompt was cancelled."
                Nothing

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
