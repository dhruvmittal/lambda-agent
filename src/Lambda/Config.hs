{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Config
  ( Config(..)
  , McpServerConfig(..)
  , defaultConfig
  , loadConfig
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:?), (.!=))
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

data McpServerConfig = McpServerConfig
  { mcpCommand :: !FilePath
  , mcpArgs    :: ![Text]
  , mcpEnv     :: !(Map Text Text)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON McpServerConfig where
  toJSON McpServerConfig{..} = Aeson.object
    [ "command" .= mcpCommand
    , "args"    .= mcpArgs
    , "env"     .= mcpEnv
    ]

instance Aeson.FromJSON McpServerConfig where
  parseJSON = Aeson.withObject "McpServerConfig" $ \obj -> do
    mcpCommand <- obj .:? "command" .!= ""
    mcpArgs    <- obj .:? "args" .!= []
    mcpEnv     <- obj .:? "env" .!= Map.empty
    pure McpServerConfig{..}

data Config = Config
  { apiBaseUrl          :: !Text
  , apiKey              :: !Text
  , modelName           :: !Text
  , customHeaders       :: !(Map Text Text)
  , alwaysAllowGlobs    :: ![String]
  , alwaysDenyGlobs     :: ![String]
  , workspaceRoot       :: !FilePath
  , artifactDir         :: !FilePath
  , maxTurnBudget       :: !Int
  , contextWindowLimit  :: !Int
  , mcpServers          :: !(Map Text McpServerConfig)
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON Config where
  toJSON Config{..} = Aeson.object
    [ "api_base_url"        .= apiBaseUrl
    , "api_key"             .= apiKey
    , "model_name"          .= modelName
    , "custom_headers"      .= customHeaders
    , "always_allow_globs"  .= alwaysAllowGlobs
    , "always_deny_globs"   .= alwaysDenyGlobs
    , "max_turn_budget"     .= maxTurnBudget
    , "context_limit"       .= contextWindowLimit
    , "mcp_servers"         .= mcpServers
    ]

instance Aeson.FromJSON Config where
  parseJSON = Aeson.withObject "Config" $ \obj -> do
    apiBaseUrl         <- obj .:? "api_base_url" .!= "https://openrouter.ai/api/v1"
    apiKey             <- obj .:? "api_key" .!= ""
    modelName          <- obj .:? "model_name" .!= "openrouter/free"
    customHeaders      <- obj .:? "custom_headers" .!= Map.empty
    alwaysAllowGlobs   <- obj .:? "always_allow_globs" .!= defaultAllowGlobs
    alwaysDenyGlobs    <- obj .:? "always_deny_globs" .!= defaultDenyGlobs
    maxTurnBudget      <- obj .:? "max_turn_budget" .!= 30
    contextWindowLimit <- obj .:? "context_limit" .!= 128000
    mcpServers         <- obj .:? "mcp_servers" .!= Map.empty
    let workspaceRoot = "."
        artifactDir   = ".lambda/artifacts"
    pure Config{..}

defaultAllowGlobs :: [String]
defaultAllowGlobs =
  [ "git status*"
  , "git diff*"
  , "git log*"
  , "ls*"
  , "pwd*"
  , "cat *"
  , "read_file*"
  , "list_directory*"
  , "fetch_url*"
  , "sd_*"
  ]

defaultDenyGlobs :: [String]
defaultDenyGlobs =
  [ "rm -rf /*"
  , "mkfs*"
  , ":(){ :|:& };:"
  ]

defaultConfig :: Config
defaultConfig = Config
  { apiBaseUrl          = "https://openrouter.ai/api/v1"
  , apiKey              = ""
  , modelName           = "openrouter/free"
  , customHeaders       = Map.empty
  , alwaysAllowGlobs    = defaultAllowGlobs
  , alwaysDenyGlobs     = defaultDenyGlobs
  , workspaceRoot       = "."
  , artifactDir         = ".lambda/artifacts"
  , maxTurnBudget       = 30
  , contextWindowLimit  = 128000
  , mcpServers          = Map.empty
  }

loadConfig :: FilePath -> IO Config
loadConfig wsRoot = do
  home <- getHomeDirectory
  let globalPath = home </> ".config" </> "lambdA" </> "config.json"
      localPath  = wsRoot </> ".lambda" </> "config.json"

  baseCfg <- do
    gExists <- doesFileExist globalPath
    if gExists
      then do
        content <- BL.readFile globalPath
        pure $ case Aeson.decode content of
          Just c  -> c
          Nothing -> defaultConfig
      else pure defaultConfig

  mergedCfg <- do
    lExists <- doesFileExist localPath
    if lExists
      then do
        content <- BL.readFile localPath
        pure $ case Aeson.decode content of
          Just c  -> c
          Nothing -> baseCfg
      else pure baseCfg

  -- Load local .env / .env.local variables if present
  dotEnvMap <- do
    let localEnvPath = wsRoot </> ".env.local"
        envPath      = wsRoot </> ".env"
    hasLocal <- doesFileExist localEnvPath
    hasEnv   <- doesFileExist envPath
    raw <- if hasLocal
      then TIO.readFile localEnvPath
      else if hasEnv
        then TIO.readFile envPath
        else pure ""
    pure $ Map.fromList (parseDotEnv raw)

  let lookupVar name = do
        mEnv <- lookupEnv name
        pure $ case mEnv of
          Just v | not (null v) -> Just v
          _                     -> Map.lookup name dotEnvMap

  -- Environment variable overrides with fallbacks for OpenRouter & OpenAI
  mEnvKey <- lookupVar "LAMBDA_API_KEY" >>= \case
    Just k | not (null k) -> pure (Just k)
    _ -> lookupVar "OPENROUTER_API_KEY" >>= \case
      Just k | not (null k) -> pure (Just k)
      _ -> lookupVar "OPENAI_API_KEY"

  mEnvUrl <- lookupVar "LAMBDA_BASE_URL" >>= \case
    Just u | not (null u) -> pure (Just u)
    _ -> lookupVar "OPENAI_BASE_URL"

  mEnvMod <- lookupVar "LAMBDA_MODEL" >>= \case
    Just m | not (null m) -> pure (Just m)
    _ -> lookupVar "OPENROUTER_MODEL" >>= \case
      Just m | not (null m) -> pure (Just m)
      _ -> lookupVar "OPENAI_MODEL"

  mEnvLimit <- lookupVar "CONTEXT_LIMIT"
  let finalLimit = case mEnvLimit of
        Just l | [(n, "")] <- reads l, n > 0 -> n
        _ -> contextWindowLimit mergedCfg

  let finalKey = maybe (apiKey mergedCfg) T.pack mEnvKey
      finalUrl = maybe (apiBaseUrl mergedCfg) T.pack mEnvUrl
      finalMod = maybe (modelName mergedCfg) T.pack mEnvMod
      artDir   = wsRoot </> ".lambda" </> "artifacts"

  createDirectoryIfMissing True artDir
  createDirectoryIfMissing True (home </> ".config" </> "lambdA")

  pure mergedCfg
    { apiBaseUrl         = finalUrl
    , apiKey             = finalKey
    , modelName          = finalMod
    , workspaceRoot      = wsRoot
    , artifactDir        = artDir
    , contextWindowLimit = finalLimit
    }

-- | Simple parser for KEY=VALUE pairs in .env / .env.local files
parseDotEnv :: Text -> [(String, String)]
parseDotEnv raw =
  [ (T.unpack (T.strip k), stripQuotes (T.unpack (T.strip (T.drop 1 rest))))
  | line <- T.lines raw
  , let trimmed = T.strip line
  , not (T.null trimmed)
  , not ("#" `T.isPrefixOf` trimmed)
  , let withoutExport = if "export " `T.isPrefixOf` trimmed then T.drop 7 trimmed else trimmed
  , let (k, rest) = T.breakOn "=" withoutExport
  , not (T.null rest)
  ]
  where
    stripQuotes s = case s of
      ('"':xs) | not (null xs) && last xs == '"' -> init xs
      ('\'':xs) | not (null xs) && last xs == '\'' -> init xs
      _ -> s
