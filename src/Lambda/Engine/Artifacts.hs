{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Artifacts
  ( spooDiagnosticArtifact
  , formatArtifactPointer
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | Spools raw output out-of-band if it exceeds the threshold (500 chars or 40 lines).
-- Returns a compact summary string along with the optional artifact file path.
spooDiagnosticArtifact :: FilePath -> Text -> Text -> IO (Text, Maybe FilePath)
spooDiagnosticArtifact artifactDir toolName rawOutput = do
  let lineCount = length (T.lines rawOutput)
      charCount = T.length rawOutput

  if charCount > 500 || lineCount > 40
    then do
      createDirectoryIfMissing True artifactDir
      ts <- show . (round :: Double -> Integer) . realToFrac <$> getPOSIXTime
      let filename = T.unpack toolName <> "_" <> ts <> ".log"
          fullPath = artifactDir </> filename
      BS.writeFile fullPath (TE.encodeUtf8 rawOutput)

      -- Generate universal head/tail window summary
      let ls = T.lines rawOutput
          headLines = take 15 ls
          tailLines = drop (max 0 (length ls - 25)) ls
          summaryText = T.unlines
            [ T.unlines headLines
            , "... [" <> T.pack (show (lineCount - 40)) <> " lines omitted. Full dump spooled out-of-band] ..."
            , T.unlines tailLines
            ]
          pointer = formatArtifactPointer (T.pack filename) fullPath summaryText lineCount
      pure (pointer, Just fullPath)
    else
      pure (rawOutput, Nothing)

-- | Formats an invariant artifact pointer handle for prompt ingestion
formatArtifactPointer :: Text -> FilePath -> Text -> Int -> Text
formatArtifactPointer artId path synopsis totalLines =
  T.unlines
    [ "<artifact_pointer id=\"" <> artId <> "\" path=\"" <> T.pack path <> "\" total_lines=\"" <> T.pack (show totalLines) <> "\">"
    , synopsis
    , "</artifact_pointer>"
    ]
