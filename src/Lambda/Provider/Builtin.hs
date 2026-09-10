{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Provider.Builtin
  ( builtinTools
  , bashTool
  , readFileTool
  , writeFileTool
  , editFileTool
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.Process.Typed
import System.Timeout (timeout)

import Lambda.Core.ToolProvider
import Lambda.Engine.Artifacts (spooDiagnosticArtifact)
import Lambda.Types

builtinTools :: FilePath -> FilePath -> [ToolDefinition]
builtinTools wsRoot artDir =
  [ bashTool wsRoot artDir
  , readFileTool wsRoot
  , writeFileTool wsRoot
  , editFileTool wsRoot
  ]

-- | Bash execution tool with timeout and OOB artifact spooling
bashTool :: FilePath -> FilePath -> ToolDefinition
bashTool wsRoot artDir = ToolDefinition
  { toolName = "bash"
  , toolDescription = "Execute a bash shell command in the workspace directory. Large outputs are automatically spooled out-of-band to artifacts."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "command" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("The exact command line string to execute." :: Text)
              ]
          ]
      , "required" .= (["command"] :: [Text])
      ]
  , toolCapability = Destructive
  , toolExecute = \_caller args -> do
      case parseEither (Aeson.withObject "bash" (.: "command")) args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right cmd -> do
          let pConf = setStdin closed
                    $ setStdout byteStringOutput
                    $ setStderr byteStringOutput
                    $ setWorkingDir wsRoot
                    $ shell (T.unpack cmd)
          res <- try $ timeout (120 * 1000000) $ readProcess pConf
          case res of
            Left (ex :: SomeException) ->
              pure $ ToolResult "" "" ("Execution exception: " <> T.pack (show ex)) Nothing
            Right Nothing ->
              pure $ ToolResult "" "" "Command timed out after 120 seconds." Nothing
            Right (Just (exitCode, outBs, errBs)) -> do
              let outText = TE.decodeUtf8Lenient (BS.toStrict outBs)
                  errText = TE.decodeUtf8Lenient (BS.toStrict errBs)
                  combined = if T.null errText then outText else outText <> "\nSTDERR:\n" <> errText

              (compactOutput, mArtifact) <- spooDiagnosticArtifact artDir "bash" combined

              let exitStatus = case exitCode of
                    ExitSuccess -> ""
                    ExitFailure code -> "\n[Process exited with code " <> T.pack (show code) <> "]"

              pure $ ToolResult "" (compactOutput <> exitStatus) errText mArtifact
  }

-- | File reading tool with optional line slicing
readFileTool :: FilePath -> ToolDefinition
readFileTool wsRoot = ToolDefinition
  { toolName = "read_file"
  , toolDescription = "Read the contents of a file with optional start_line and line_count parameters."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "path" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to file relative to workspace." :: Text)
              ]
          , "start_line" .= Aeson.object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("1-based start line (optional)." :: Text)
              ]
          , "line_count" .= Aeson.object
              [ "type" .= ("integer" :: Text)
              , "description" .= ("Maximum number of lines to read (optional)." :: Text)
              ]
          ]
      , "required" .= (["path"] :: [Text])
      ]
  , toolCapability = ReadOnly
  , toolExecute = \_caller args -> do
      let parseArgs = Aeson.withObject "read_file" $ \o -> do
            p <- o .: "path"
            s <- o .:? "start_line"
            c <- o .:? "line_count"
            pure (p, s, c)
      case parseEither parseArgs args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right (relPath, mStart, mCount) -> do
          let fullPath = wsRoot </> T.unpack relPath
          exists <- doesFileExist fullPath
          if not exists
            then pure $ ToolResult "" "" ("File not found: " <> relPath) Nothing
            else do
              fileContent <- TIO.readFile fullPath
              let allLines = T.lines fileContent
                  totalLines = length allLines
                  sIdx = maybe 0 (\s -> max 0 (s - 1)) mStart
                  cCount = maybe (totalLines - sIdx) (max 0) mCount
                  selectedLines = take cCount (drop sIdx allLines)
                  numbered = zipWith (\n l -> T.pack (show n) <> ": " <> l) [sIdx + 1 ..] selectedLines
              pure $ ToolResult "" (T.unlines numbered) "" Nothing
  }

-- | File writing tool
writeFileTool :: FilePath -> ToolDefinition
writeFileTool wsRoot = ToolDefinition
  { toolName = "write_file"
  , toolDescription = "Write full content to a file in the workspace, creating parent directories if necessary."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "path" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to file relative to workspace." :: Text)
              ]
          , "content" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Full content to write." :: Text)
              ]
          ]
      , "required" .= (["path", "content"] :: [Text])
      ]
  , toolCapability = Destructive
  , toolExecute = \_caller args -> do
      let parseArgs = Aeson.withObject "write_file" $ \o -> do
            p <- o .: "path"
            c <- o .: "content"
            pure (p, c)
      case parseEither parseArgs args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right (relPath, content) -> do
          let fullPath = wsRoot </> T.unpack relPath
          createDirectoryIfMissing True (takeDirectory fullPath)
          TIO.writeFile fullPath content
          pure $ ToolResult "" ("File successfully written: " <> relPath) "" Nothing
  }

-- | File editing tool (target text replacement)
editFileTool :: FilePath -> ToolDefinition
editFileTool wsRoot = ToolDefinition
  { toolName = "edit_file"
  , toolDescription = "Replace an exact target text sequence with new replacement text in a file."
  , toolParameters = Aeson.object
      [ "type" .= ("object" :: Text)
      , "properties" .= Aeson.object
          [ "path" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Path to file relative to workspace." :: Text)
              ]
          , "target" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("Exact text to replace." :: Text)
              ]
          , "replacement" .= Aeson.object
              [ "type" .= ("string" :: Text)
              , "description" .= ("New replacement text." :: Text)
              ]
          ]
      , "required" .= (["path", "target", "replacement"] :: [Text])
      ]
  , toolCapability = Destructive
  , toolExecute = \_caller args -> do
      let parseArgs = Aeson.withObject "edit_file" $ \o -> do
            p <- o .: "path"
            t <- o .: "target"
            r <- o .: "replacement"
            pure (p, t, r)
      case parseEither parseArgs args of
        Left err -> pure $ ToolResult "" "" ("Invalid arguments: " <> T.pack err) Nothing
        Right (relPath, target, replacement) -> do
          let fullPath = wsRoot </> T.unpack relPath
          exists <- doesFileExist fullPath
          if not exists
            then pure $ ToolResult "" "" ("File not found: " <> relPath) Nothing
            else do
              content <- TIO.readFile fullPath
              if not (target `T.isInfixOf` content)
                then pure $ ToolResult "" "" "Target string not found in file. Edit aborted." Nothing
                else do
                  let updated = T.replace target replacement content
                  TIO.writeFile fullPath updated
                  pure $ ToolResult "" ("Successfully edited file: " <> relPath) "" Nothing
  }
