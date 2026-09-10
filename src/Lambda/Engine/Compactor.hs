{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.Engine.Compactor
  ( sanitizeForApiPayload
  , pruneHistoricalThinking
  , turnsToOpenAIPayload
  , isThinkingBlock
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lambda.Types

-- | Strips raw internal reasoning blocks from previous turns before remote API wire egress.
-- Current active turn keeps reasoning so the model maintains its local deduction scratchpad.
sanitizeForApiPayload :: [Turn] -> [Turn]
sanitizeForApiPayload [] = []
sanitizeForApiPayload turns =
  let total = length turns
      sanitizeTurn idx turn
        | idx == total - 1 = turn
        | otherwise        = pruneHistoricalThinking turn
  in zipWith sanitizeTurn [0..] turns

pruneHistoricalThinking :: Turn -> Turn
pruneHistoricalThinking turn =
  turn { turnBlocks = filter (not . isThinkingBlock) (turnBlocks turn) }

isThinkingBlock :: ContentBlock -> Bool
isThinkingBlock (ThinkingBlock {}) = True
isThinkingBlock _                  = False

-- | Converts dialogue turns into standard OpenAI wire-protocol JSON messages.
-- Correctly generates native assistant `tool_calls` and `tool` role responses!
turnsToOpenAIPayload :: [Turn] -> [Aeson.Value]
turnsToOpenAIPayload turns =
  let sanitized = sanitizeForApiPayload turns
  in concatMap turnToMessages sanitized
  where
    turnToMessages :: Turn -> [Aeson.Value]
    turnToMessages (Turn _ role blocks) =
      case role of
        SystemRole ->
          [ Aeson.object
              [ "role"    .= ("system" :: Text)
              , "content" .= renderTextBlocks blocks
              ]
          ]
        UserRole ->
          [ Aeson.object
              [ "role"    .= ("user" :: Text)
              , "content" .= renderTextBlocks blocks
              ]
          ]
        AssistantRole ->
          let toolCalls = mapMaybe extractToolCall blocks
              textContent = renderTextBlocks blocks
          in if null toolCalls
               then [ Aeson.object
                        [ "role"    .= ("assistant" :: Text)
                        , "content" .= textContent
                        ]
                    ]
               else [ Aeson.object
                        [ "role"       .= ("assistant" :: Text)
                        , "content"    .= if T.null textContent then Aeson.Null else Aeson.toJSON textContent
                        , "tool_calls" .= map Aeson.toJSON toolCalls
                        ]
                    ]
        ToolRole ->
          mapMaybe toolResultToMessage blocks

    extractToolCall (ToolCallBlock tc) = Just tc
    extractToolCall _                  = Nothing

    toolResultToMessage (ToolResultBlock ToolResult{..}) =
      Just $ Aeson.object
        [ "role"         .= ("tool" :: Text)
        , "tool_call_id" .= resultCallIdRef
        , "content"      .= resultStdout
        ]
    toolResultToMessage _ = Nothing

    renderTextBlocks blks =
      T.concat [ t | TextBlock t <- blks ]
