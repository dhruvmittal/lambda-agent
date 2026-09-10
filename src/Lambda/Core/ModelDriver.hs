module Lambda.Core.ModelDriver
  ( ModelDriver(..)
  ) where

import qualified Data.Aeson as Aeson
import Lambda.Types

-- | Abstract interface for LLM streaming completions and tool schemas
data ModelDriver = ModelDriver
  { streamCompletion
      :: [Turn]                     -- Sanitized conversation turns
      -> [Aeson.Value]               -- Available tool schemas
      -> (StreamChunk -> IO ())      -- Stream callback for chunks
      -> IO ()
  }
