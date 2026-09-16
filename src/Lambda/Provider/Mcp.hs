{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lambda.Provider.Mcp
  ( McpClient(..)
  , startMcpClient
  , stopMcpClient
  , loadMcpTools
  , startAndLoadMcpServers
  , inferCapability
  , parseMcpCallResult
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (findExecutable, doesFileExist, getHomeDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))

import Lambda.Config (McpServerConfig(..))
import Lambda.Core.ToolProvider
import Lambda.Engine.Artifacts (spoolDiagnosticArtifact)
import Lambda.Provider.JsonRpc
import Lambda.Types

data McpClient = McpClient
  { mcpRpcClient  :: !JsonRpcClient
  , mcpServerName :: !Text
  }

-- | Launches an MCP client process and performs the standard MCP handshake (initialize + initialized notification)
startMcpClient :: Text -> FilePath -> [Text] -> Map Text Text -> IO (Either Text McpClient)
startMcpClient name cmd args envMap = do
  home <- getHomeDirectory
  resolvedCmd <- do
    mExe <- findExecutable cmd
    case mExe of
      Just p  -> pure p
      Nothing -> do
        let npmCandidate = home </> ".npm-global" </> "bin" </> cmd
            locCandidate = home </> ".local" </> "bin" </> cmd
        npmExists <- doesFileExist npmCandidate
        if npmExists
          then pure npmCandidate
          else do
            locExists <- doesFileExist locCandidate
            if locExists then pure locCandidate else pure cmd

  currEnv <- getEnvironment
  let currPath = maybe "" id (lookup "PATH" currEnv)
      npmBin = home </> ".npm-global" </> "bin"
      augmentedPath = if T.pack npmBin `T.isInfixOf` T.pack currPath
                        then currPath
                        else npmBin <> (if null currPath then "" else ":" <> currPath)
      envList = case Map.lookup "PATH" envMap of
        Just p  -> ( "PATH", T.unpack p ) : [ (T.unpack k, T.unpack v) | (k, v) <- Map.toList envMap, k /= "PATH" ]
        Nothing -> ( "PATH", augmentedPath ) : [ (T.unpack k, T.unpack v) | (k, v) <- Map.toList envMap ]

  rpcClientRes <- try $ startRpcClientWithEnv LineFramed resolvedCmd args envList
  case rpcClientRes of
    Left (ex :: SomeException) ->
      pure $ Left $ "Failed to spawn MCP server process '" <> name <> "': " <> T.pack (show ex)
    Right rpcClient -> do
      -- Step 1: Send initialize request
      let initParams = Aeson.object
            [ "protocolVersion" .= ("2024-11-05" :: Text)
            , "capabilities"    .= Aeson.object
                [ "roots"    .= Aeson.object [ "listChanged" .= True ]
                , "sampling" .= Aeson.object []
                ]
            , "clientInfo"      .= Aeson.object
                [ "name"    .= ("lambdA" :: Text)
                , "version" .= ("0.1.0" :: Text)
                ]
            ]
      initRes <- sendRequest rpcClient "initialize" initParams
      case initRes of
        Left err -> do
          stopRpcClient rpcClient
          pure $ Left $ "MCP handshake failed for server '" <> name <> "': " <> err
        Right _ -> do
          -- Step 2: Send notifications/initialized
          sendNotification rpcClient "notifications/initialized" (Aeson.object [])
          pure $ Right McpClient
            { mcpRpcClient  = rpcClient
            , mcpServerName = name
            }

stopMcpClient :: McpClient -> IO ()
stopMcpClient McpClient{..} = stopRpcClient mcpRpcClient

-- | Queries the MCP server for available tools via tools/list and wraps them into ToolDefinition
loadMcpTools :: McpClient -> IO [ToolDefinition]
loadMcpTools McpClient{..} = do
  res <- sendRequest mcpRpcClient "tools/list" (Aeson.object [])
  case res of
    Left _err -> pure []
    Right (Aeson.Object obj) ->
      case parseEither (.: "tools") obj of
        Right (toolsList :: [Aeson.Value]) ->
          pure $ mapMaybe (parseMcpTool mcpServerName mcpRpcClient) toolsList
        _ -> pure []
    _ -> pure []

-- | Parses a single MCP tool definition object and binds tools/call execution
parseMcpTool :: Text -> JsonRpcClient -> Aeson.Value -> Maybe ToolDefinition
parseMcpTool _serverName rpcClient (Aeson.Object obj) = do
  tName <- case parseEither (.: "name") obj of
    Right n -> Just n
    _       -> Nothing
  let tDesc = case parseEither (.: "description") obj of
        Right d -> d
        _       -> ""
      tSchema = case parseEither (.: "inputSchema") obj of
        Right s -> s
        _       -> Aeson.object ["type" .= ("object" :: Text)]
      tCap = inferCapability tName tDesc

  Just ToolDefinition
    { toolName        = tName
    , toolDescription = tDesc
    , toolParameters  = tSchema
    , toolCapability  = tCap
    , toolExecute     = \_caller argsVal -> do
        callRes <- sendRequest rpcClient "tools/call" $ Aeson.object
          [ "name"      .= tName
          , "arguments" .= argsVal
          ]
        case callRes of
          Left err -> pure $ ToolResult "" "" ("MCP tool execution error (" <> tName <> "): " <> err) Nothing
          Right resultVal -> do
            let (outText, isErr) = parseMcpCallResult resultVal
            if isErr
              then pure $ ToolResult "" "" outText Nothing
              else do
                (renderedOut, mArtPath) <- spoolDiagnosticArtifact ".lambda/artifacts" ("mcp_" <> tName) outText
                pure $ ToolResult "" renderedOut "" mArtPath
    }
parseMcpTool _ _ _ = Nothing

-- | Extracts text content and error flag from an MCP tools/call result payload
parseMcpCallResult :: Aeson.Value -> (Text, Bool)
parseMcpCallResult (Aeson.Object obj) =
  let isErr = case parseEither (.: "isError") obj of
        Right b -> b
        _       -> False
      contentBlocks = case parseEither (.: "content") obj of
        Right (items :: [Aeson.Value]) ->
          [ txt | Aeson.Object item <- items
                , parseEither (.: "type") item == Right ("text" :: Text)
                , Right txt <- [parseEither (.: "text") item]
          ]
        _ -> []
      combinedText = if null contentBlocks
        then case parseEither (.: "text") obj of
               Right t -> t
               _       -> "(no text content in MCP response)"
        else T.intercalate "\n" contentBlocks
  in (combinedText, isErr)
parseMcpCallResult (Aeson.String s) = (s, False)
parseMcpCallResult val = (TE.decodeUtf8 (BL.toStrict (Aeson.encode val)), False)

-- | Heuristically infers ReadOnly vs Destructive capability for MCP tools
inferCapability :: Text -> Text -> ToolCapability
inferCapability name desc
  | isMutatingName = Destructive
  | isReadName || isReadDesc = ReadOnly
  | otherwise = Destructive
  where
    nLower = T.toLower name
    dLower = T.toLower desc
    isMutatingName = any (`T.isInfixOf` nLower)
      [ "delete", "write", "remove", "add", "create", "update", "modify", "patch", "replace", "drop", "format", "kill", "prune", "consolidate" ]
    isReadName = any (`T.isPrefixOf` nLower)
      [ "read", "get", "list", "search", "recall", "find", "check", "inspect", "show", "view", "sd_read", "sd_recall", "sd_search", "sd_get", "nix" ]
    isReadDesc = "read-only" `T.isInfixOf` dLower || "inspect" `T.isInfixOf` dLower

-- | Initializes all configured MCP servers, loads their tools, and returns active clients and definitions
startAndLoadMcpServers :: Map Text McpServerConfig -> IO ([McpClient], [ToolDefinition])
startAndLoadMcpServers servers = do
  results <- mapM (uncurry initOne) (Map.toList servers)
  let (clients, toolsLists) = foldr step ([], []) results
  pure (clients, concat toolsLists)
  where
    initOne name McpServerConfig{..}
      | null mcpCommand = pure (Nothing, [])
      | otherwise = do
          res <- startMcpClient name mcpCommand mcpArgs mcpEnv
          case res of
            Left err -> do
              putStrLn $ "[MCP Warning] Failed to initialize server '" <> T.unpack name <> "': " <> T.unpack err
              pure (Nothing, [])
            Right client -> do
              tls <- loadMcpTools client
              pure (Just client, tls)

    step (Just c, tls) (accC, accT) = (c : accC, tls : accT)
    step (Nothing, _)  acc          = acc
