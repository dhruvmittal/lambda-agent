{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Config
  ( Config(..)
  , McpServerConfig(..)
  , SpecialistConfig(..)
  , defaultConfig
  , defaultSpecialists
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

data SpecialistConfig = SpecialistConfig
  { specialistDescription  :: !Text
  , specialistPrompt       :: !Text
  , specialistBudget       :: !Int
  , specialistCapabilities :: ![Text]
  } deriving stock (Eq, Show, Generic)

instance Aeson.ToJSON SpecialistConfig where
  toJSON SpecialistConfig{..} = Aeson.object
    [ "description"  .= specialistDescription
    , "prompt"       .= specialistPrompt
    , "budget"       .= specialistBudget
    , "capabilities" .= specialistCapabilities
    ]

instance Aeson.FromJSON SpecialistConfig where
  parseJSON = Aeson.withObject "SpecialistConfig" $ \obj -> do
    specialistDescription  <- obj .:? "description" .!= ""
    specialistPrompt       <- obj .:? "prompt" .!= ""
    specialistBudget       <- obj .:? "budget" .!= 6
    specialistCapabilities <- obj .:? "capabilities" .!= []
    pure SpecialistConfig{..}

defaultSpecialists :: Map Text SpecialistConfig
defaultSpecialists = Map.fromList
  [ ( "surveyor"
    , SpecialistConfig
        { specialistDescription  = "Read-only codebase explorer & caller graph mapper"
        , specialistPrompt       = T.unlines
            [ "# Role: Codebase Surveyor & Architecture Mapper"
            , "You are a fastidious, read-only codebase explorer and topological mapping specialist."
            , "Your mission is to map symbols, locate definitions, trace imports, and verify architectural invariants."
            , "Operational Invariants:"
            , "1. STRICT READ-ONLY: Never modify workspace files. Only read-only tools are permitted."
            , "2. NO PATH HALLUCINATION: Never guess paths or symbols. Always verify via find_by_name, grep_search, or sd_read/sd_recall."
            , "3. PRECISE REFERENCES: Return exact file paths and line ranges (e.g. src/Foo.hs:L40-L65)."
            , "When your survey is complete, call `submit_report` with status, summary, details, and any artifact path."
            ]
        , specialistBudget       = 16
        , specialistCapabilities = ["read_file*", "list_directory*", "grep_search*", "find_by_name*", "fetch_url*", "sd_*", "git status*", "git diff*", "git log*"]
        }
    )
  , ( "debugger"
    , SpecialistConfig
        { specialistDescription  = "Forensic bug investigator & minimal test reproducer"
        , specialistPrompt       = T.unlines
            [ "# Role: Forensic Systems Debugger"
            , "You are a forensic debugging specialist. Your mission is to isolate the root-cause of crashes, test failures, and regressions."
            , "Operational Invariants:"
            , "1. HYPOTHESIS TESTING: Formulate distinct, testable hypotheses for the failure mechanism."
            , "2. STACK ISOLATION: Locate the exact offending line, frame, or memory violation."
            , "3. MINIMAL SURGERY: Diagnose the root cause and propose the minimal surgical fix required."
            , "When your diagnosis is complete, call `submit_report` with status, summary, details, and any artifact path."
            ]
        , specialistBudget       = 14
        , specialistCapabilities = ["read_file*", "grep_search*", "find_by_name*", "cabal test*", "ctest*", "pytest*", "bash*", "sd_*"]
        }
    )
  , ( "profiler"
    , SpecialistConfig
        { specialistDescription  = "Systems performance, Valgrind, and hotspot analyzer"
        , specialistPrompt       = T.unlines
            [ "# Role: Systems Performance Profiler"
            , "You are a systems performance profiling specialist. Your mission is to benchmark and identify CPU, memory, and cache bottlenecks."
            , "Operational Invariants:"
            , "1. METRIC PRECISION: Run profilers (valgrind callgrind/massif, perf, RTS profiling) and extract instruction/cycle metrics."
            , "2. HOTSPOT ISOLATION: Identify the top 3 functions consuming the majority of time/allocations."
            , "3. CONCRETE REMEDY: Recommend specific optimizations (e.g. allocation pre-sizing, unboxed structures, memory reuse)."
            , "When profiling is complete, call `submit_report` with status, summary, details, and the profile artifact path."
            ]
        , specialistBudget       = 10
        , specialistCapabilities = ["valgrind*", "callgrind_annotate*", "perf*", "read_file*", "bash*", "cabal bench*"]
        }
    )
  , ( "implementer"
    , SpecialistConfig
        { specialistDescription  = "Surgical file modifier & refactoring implementer"
        , specialistPrompt       = T.unlines
            [ "# Role: Surgical Implementation Specialist"
            , "You are an implementation specialist. Your mission is to execute clean, minimal file modifications based on approved plans."
            , "Operational Invariants:"
            , "1. SURGICAL PRECISION: Make only the targeted changes required. Do not refactor unrelated code."
            , "2. PRESERVE DOCUMENTATION: Never delete comments, docstrings, or existing architectural conventions."
            , "3. VERIFY DIFFS: Check your changes for syntax correctness and clean formatting."
            , "When changes are complete, call `submit_report` with status, summary, details, and the modified file paths."
            ]
        , specialistBudget       = 12
        , specialistCapabilities = ["write_file*", "replace_lines*", "read_file*", "sd_read*"]
        }
    )
  , ( "reviewer"
    , SpecialistConfig
        { specialistDescription  = "Adversarial pre-commit invariant and simplicity auditor"
        , specialistPrompt       = T.unlines
            [ "# Role: Adversarial Code Reviewer"
            , "You are an adversarial systems code reviewer. Your mission is to verify safety, correctness, and simplicity before code lands."
            , "Operational Invariants:"
            , "1. INVARIANT CHECKING: Inspect git diffs against architectural invariants, concurrency safety, and memory management."
            , "2. YAGNI & SIMPLICITY: Hunt for over-engineering, unneeded dependencies, speculative abstractions, and dead flexibility."
            , "3. ACTIONABLE VERDICT: Issue APPROVED or CHANGES_REQUESTED with precise line-by-line feedback."
            , "When review is complete, call `submit_report` with status, summary, details, and any review artifacts."
            ]
        , specialistBudget       = 12
        , specialistCapabilities = ["git diff*", "git log*", "read_file*", "cabal test*", "ctest*", "sd_recall*"]
        }
    )
  ]

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
  , specialists         :: !(Map Text SpecialistConfig)
  , maxSavedSessions    :: !Int
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
    , "specialists"         .= specialists
    , "max_saved_sessions"  .= maxSavedSessions
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
    userSpecialists    <- obj .:? "specialists" .!= Map.empty
    userSubagents      <- obj .:? "subagents" .!= Map.empty
    maxSavedSessions   <- obj .:? "max_saved_sessions" .!= 50
    let specialists = Map.union userSpecialists (Map.union userSubagents defaultSpecialists)
        workspaceRoot  = "."
        artifactDir    = ".lambda/artifacts"
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
  , "spawn_specialist_subagent*"
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
  , specialists         = defaultSpecialists
  , maxSavedSessions    = 50
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
