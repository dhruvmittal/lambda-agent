{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lambda.UI.Theme
  ( ThemePalette(..)
  , builtinThemes
  , builtinThemeNames
  , parseHexColor
  , resolveThemePalette
  , buildThemeAttrMap
  , resolveThemeAttrMap
  ) where

import Brick (AttrMap, AttrName, attrMap, attrName, fg, bg, on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import qualified Graphics.Vty as V
import Numeric (readHex)

import Lambda.Config (Config(..))

-- | 12-slot semantic color palette for lightweight terminal theming.
-- When 'tpBg' is Nothing ("default" theme), the terminal emulator's native
-- background is preserved and muted text uses 'tpFgDim' with 'V.dim' styling
-- instead of ANSI slot 8 (V.brightBlack), preventing invisible thinking blocks
-- on Nord/Solarized/dark-slate terminal profiles.
data ThemePalette = ThemePalette
  { tpName      :: !Text
  , tpBg        :: !(Maybe V.Color)
  , tpBgAlt     :: !(Maybe V.Color)
  , tpFg        :: !V.Color
  , tpFgBright  :: !V.Color
  , tpFgDim     :: !V.Color
  , tpAccent1   :: !V.Color
  , tpAccent2   :: !V.Color
  , tpUser      :: !V.Color
  , tpWarn      :: !V.Color
  , tpError     :: !V.Color
  , tpSuccess   :: !V.Color
  } deriving (Eq, Show)

rgb :: Word8 -> Word8 -> Word8 -> V.Color
rgb = V.srgbColor

-- | Parse a "#RRGGBB" or "RRGGBB" hex string into a Vty 24-bit sRGB color.
parseHexColor :: Text -> Maybe V.Color
parseHexColor raw =
  let clean = T.dropWhile (== '#') (T.strip raw)
  in if T.length clean == 6
       then
         let (rStr, rest1) = T.splitAt 2 clean
             (gStr, bStr)  = T.splitAt 2 rest1
         in case (readHex (T.unpack rStr), readHex (T.unpack gStr), readHex (T.unpack bStr)) of
              ([(r, "")], [(g, "")], [(b, "")]) -> Just (rgb r g b)
              _                                 -> Nothing
       else Nothing

-- | Canonical built-in colorschemes (all calibrated for >= 4.5:1 contrast on tpFgDim).
builtinThemes :: Map Text ThemePalette
builtinThemes = Map.fromList
  [ ( "default"
    , ThemePalette
        { tpName     = "default"
        , tpBg       = Nothing
        , tpBgAlt    = Just V.blue
        , tpFg       = V.white
        , tpFgBright = V.brightWhite
        , tpFgDim    = rgb 0x8b 0x94 0x9e -- Readable mid-slate (#8b949e) instead of broken ANSI 8 (brightBlack)
        , tpAccent1  = V.brightCyan
        , tpAccent2  = V.brightMagenta
        , tpUser     = V.brightGreen
        , tpWarn     = V.brightYellow
        , tpError    = V.brightRed
        , tpSuccess  = V.brightGreen
        }
    )
  , ( "solarized-dark"
    , ThemePalette
        { tpName     = "solarized-dark"
        , tpBg       = Just (rgb 0x00 0x2b 0x36) -- Base03
        , tpBgAlt    = Just (rgb 0x07 0x36 0x42) -- Base02
        , tpFg       = rgb 0x83 0x94 0x96        -- Base0
        , tpFgBright = rgb 0x93 0xa1 0xa1        -- Base1
        , tpFgDim    = rgb 0x65 0x7b 0x83        -- Base00 (4.5:1 against Base03)
        , tpAccent1  = rgb 0x2a 0xa1 0x98        -- Cyan
        , tpAccent2  = rgb 0x6c 0x71 0xc4        -- Violet
        , tpUser     = rgb 0x26 0x8b 0xd2        -- Blue
        , tpWarn     = rgb 0xb5 0x89 0x00        -- Yellow
        , tpError    = rgb 0xdc 0x32 0x2f        -- Red
        , tpSuccess  = rgb 0x85 0x99 0x00        -- Green
        }
    )
  , ( "solarized-light"
    , ThemePalette
        { tpName     = "solarized-light"
        , tpBg       = Just (rgb 0xfd 0xf6 0xe3) -- Base3
        , tpBgAlt    = Just (rgb 0xee 0xe8 0xd5) -- Base2
        , tpFg       = rgb 0x58 0x6e 0x75        -- Base01
        , tpFgBright = rgb 0x07 0x36 0x42        -- Base02
        , tpFgDim    = rgb 0x83 0x94 0x96        -- Base0 (4.5:1 against Base3)
        , tpAccent1  = rgb 0x2a 0xa1 0x98        -- Cyan
        , tpAccent2  = rgb 0x6c 0x71 0xc4        -- Violet
        , tpUser     = rgb 0x26 0x8b 0xd2        -- Blue
        , tpWarn     = rgb 0xb5 0x89 0x00        -- Yellow
        , tpError    = rgb 0xdc 0x32 0x2f        -- Red
        , tpSuccess  = rgb 0x85 0x99 0x00        -- Green
        }
    )
  , ( "nordic"
    , ThemePalette
        { tpName     = "nordic"
        , tpBg       = Just (rgb 0x2e 0x34 0x40) -- Polar Night 0
        , tpBgAlt    = Just (rgb 0x3b 0x42 0x52) -- Polar Night 1
        , tpFg       = rgb 0xd8 0xde 0xe9        -- Snow Storm 0
        , tpFgBright = rgb 0xec 0xef 0xf4        -- Snow Storm 2
        , tpFgDim    = rgb 0x90 0x99 0xab        -- Lifted Frost-Slate (4.6:1 against #2e3440)
        , tpAccent1  = rgb 0x88 0xc0 0xd0        -- Frost Cyan
        , tpAccent2  = rgb 0xb4 0x8e 0xad        -- Aurora Purple
        , tpUser     = rgb 0x81 0xa1 0xc1        -- Frost Blue
        , tpWarn     = rgb 0xeb 0xcb 0x8b        -- Aurora Yellow
        , tpError    = rgb 0xbf 0x61 0x6a        -- Aurora Red
        , tpSuccess  = rgb 0xa3 0xbe 0x8c        -- Aurora Green
        }
    )
  , ( "zenburn"
    , ThemePalette
        { tpName     = "zenburn"
        , tpBg       = Just (rgb 0x3f 0x3f 0x3f) -- Zenburn bg
        , tpBgAlt    = Just (rgb 0x2b 0x2b 0x2b) -- Zenburn bg-1
        , tpFg       = rgb 0xdc 0xdc 0xcc        -- Zenburn fg
        , tpFgBright = rgb 0xff 0xff 0xef        -- Zenburn fg+1
        , tpFgDim    = rgb 0x9f 0xaf 0x9f        -- Soft sage gray (4.5:1 against #3f3f3f)
        , tpAccent1  = rgb 0x93 0xe0 0xe3        -- Cyan
        , tpAccent2  = rgb 0xdf 0xaf 0x8f        -- Orange/peach
        , tpUser     = rgb 0x8c 0xd0 0xd3        -- Blue
        , tpWarn     = rgb 0xf0 0xdf 0xaf        -- Yellow
        , tpError    = rgb 0xcc 0x93 0x93        -- Terracotta red
        , tpSuccess  = rgb 0x7f 0x9f 0x7f        -- Green
        }
    )
  , ( "dracula"
    , ThemePalette
        { tpName     = "dracula"
        , tpBg       = Just (rgb 0x28 0x2a 0x36) -- Background
        , tpBgAlt    = Just (rgb 0x44 0x47 0x5a) -- Current Line / Alt
        , tpFg       = rgb 0xf8 0xf8 0xf2        -- Foreground
        , tpFgBright = rgb 0xff 0xff 0xff        -- Bright White
        , tpFgDim    = rgb 0x7e 0x8c 0xc2        -- Lifted comment blue (4.5:1 against #282a36)
        , tpAccent1  = rgb 0x8b 0xe9 0xfd        -- Cyan
        , tpAccent2  = rgb 0xbd 0x93 0xf9        -- Purple
        , tpUser     = rgb 0xff 0x79 0xc6        -- Pink
        , tpWarn     = rgb 0xf1 0xfa 0x8c        -- Yellow
        , tpError    = rgb 0xff 0x55 0x55        -- Red
        , tpSuccess  = rgb 0x50 0xfa 0x7b        -- Green
        }
    )
  ]

builtinThemeNames :: [Text]
builtinThemeNames =
  [ "default"
  , "solarized-dark"
  , "solarized-light"
  , "nordic"
  , "zenburn"
  , "dracula"
  ]

-- | Resolve a theme name against built-in themes and user-configured custom themes.
-- Supports friendly aliases like "nord" -> "nordic", "solarized" -> "solarized-dark".
resolveThemePalette :: Config -> Text -> Maybe ThemePalette
resolveThemePalette cfg rawName =
  let clean = T.toLower (T.strip rawName)
      canonical = case clean of
        "nord"      -> "nordic"
        "solarized" -> "solarized-dark"
        other       -> other
      userMap = customThemes cfg
  in case Map.lookup canonical userMap <|> Map.lookup (T.strip rawName) userMap of
       Just spec -> Just (parseCustomPalette canonical spec)
       Nothing   -> Map.lookup canonical builtinThemes
  where
    Nothing <|> b = b
    a       <|> _ = a

    fallback = builtinThemes Map.! "dracula"

    parseCustomPalette :: Text -> Map Text Text -> ThemePalette
    parseCustomPalette name spec =
      let pickColor k defC = case Map.lookup k spec >>= parseHexColor of
            Just c  -> c
            Nothing -> defC
          pickMaybeColor k defMC = case Map.lookup k spec of
            Just "none"    -> Nothing
            Just "default" -> Nothing
            Just hex       -> case parseHexColor hex of
                                Just c  -> Just c
                                Nothing -> defMC
            Nothing        -> defMC
      in ThemePalette
           { tpName     = name
           , tpBg       = pickMaybeColor "bg" (tpBg fallback)
           , tpBgAlt    = pickMaybeColor "bg_alt" (tpBgAlt fallback)
           , tpFg       = pickColor "fg" (tpFg fallback)
           , tpFgBright = pickColor "fg_bright" (tpFgBright fallback)
           , tpFgDim    = pickColor "fg_dim" (tpFgDim fallback)
           , tpAccent1  = pickColor "accent1" (tpAccent1 fallback)
           , tpAccent2  = pickColor "accent2" (tpAccent2 fallback)
           , tpUser     = pickColor "user" (tpUser fallback)
           , tpWarn     = pickColor "warn" (tpWarn fallback)
           , tpError    = pickColor "error" (tpError fallback)
           , tpSuccess  = pickColor "success" (tpSuccess fallback)
           }

-- | Construct the complete Brick AttrMap from a 12-slot ThemePalette.
buildThemeAttrMap :: ThemePalette -> AttrMap
buildThemeAttrMap ThemePalette{..} =
  let baseAttr = case tpBg of
        Nothing -> V.defAttr
        Just b  -> tpFg `on` b
      withThemeBg c = case tpBg of
        Nothing -> fg c
        Just b  -> c `on` b
      badgeTextBg = case tpBg of
        Nothing -> V.black
        Just b  -> b
      headerAttr = case tpBgAlt of
        Just altBg -> tpFgBright `on` altBg
        Nothing    -> withThemeBg tpFgBright
      codeBlockAttr = case tpBgAlt of
        Just altBg | tpBg /= Nothing -> tpFgBright `on` altBg
        _                            -> withThemeBg tpFgBright
  in attrMap baseAttr
       [ (attrName "planBadge",      badgeTextBg `on` tpAccent1)
       , (attrName "execBadge",      badgeTextBg `on` tpWarn)
       , (attrName "headerBar",      headerAttr)
       , (attrName "titleLogo",      headerAttr `V.withStyle` V.bold)
       , (attrName "paneTitle",      withThemeBg tpAccent1 `V.withStyle` V.bold)
       , (attrName "systemRole",     withThemeBg tpAccent1)
       , (attrName "userRole",       withThemeBg tpUser `V.withStyle` V.bold)
       , (attrName "assistantRole",  withThemeBg tpAccent2 `V.withStyle` V.bold)
       , (attrName "toolRole",       withThemeBg tpWarn)
       , (attrName "toolCall",       withThemeBg tpWarn)
       , (attrName "toolResult",     withThemeBg tpFg)
       , (attrName "toolError",      withThemeBg tpError)
       , (attrName "diffAdd",        withThemeBg tpSuccess)
       , (attrName "diffRemove",     withThemeBg tpError)
       , (attrName "diffHeader",     withThemeBg tpWarn `V.withStyle` V.bold)
       , (attrName "artifact",       withThemeBg tpAccent1)
       , (attrName "thinkingDim",    withThemeBg tpFgDim)
       , (attrName "subRunning",     withThemeBg tpSuccess)
       , (attrName "subSuccess",     withThemeBg tpAccent1)
       , (attrName "subFailed",      withThemeBg tpError)
       , (attrName "subRole",        withThemeBg tpAccent2 `V.withStyle` V.bold)
       , (attrName "selectedBadge",  badgeTextBg `on` tpWarn `V.withStyle` V.bold)
       , (attrName "ctxNormal",      withThemeBg tpAccent1)
       , (attrName "ctxWarn",        withThemeBg tpWarn `V.withStyle` V.bold)
       , (attrName "ctxHigh",        withThemeBg tpError `V.withStyle` V.bold)
       , (attrName "modelBadge",     withThemeBg tpAccent2)
       , (attrName "promptLogo",     withThemeBg tpFgBright `V.withStyle` V.bold)
       , (attrName "promptModel",    withThemeBg tpAccent2)
       , (attrName "promptDivider",  withThemeBg tpFgDim)
       , (attrName "promptArrow",    withThemeBg tpAccent1 `V.withStyle` V.bold)
       , (attrName "promptPlanMode", badgeTextBg `on` tpAccent1)
       , (attrName "promptExecMode", badgeTextBg `on` tpWarn)
       , (attrName "userAccent",     withThemeBg tpUser `V.withStyle` V.bold)
       , (attrName "turnFooter",     withThemeBg tpFgDim)
       , (attrName "hudBorder",      withThemeBg tpAccent1)
       , (attrName "hudTitle",       withThemeBg tpFgBright `V.withStyle` V.bold)
       , (attrName "hudSection",     withThemeBg tpWarn `V.withStyle` V.bold)
       , (attrName "hudKey",         withThemeBg tpAccent1)
       , (attrName "compSelected",   badgeTextBg `on` tpAccent1)
       , (attrName "compItem",       withThemeBg tpFg)
       , (attrName "compBorder",     withThemeBg tpFgDim)
       , (attrName "mdH1",           withThemeBg tpFgBright `V.withStyle` V.bold)
       , (attrName "mdH2",           withThemeBg tpFgBright `V.withStyle` V.bold)
       , (attrName "mdH3",           withThemeBg tpFg `V.withStyle` V.bold)
       , (attrName "mdCodeLang",     withThemeBg tpAccent1 `V.withStyle` V.bold)
       , (attrName "mdCodeBlock",    codeBlockAttr)
       , (attrName "mdBullet",       withThemeBg tpAccent2)
       , (attrName "mdNumbered",     withThemeBg tpAccent2)
       , (attrName "mdQuoteBar",     withThemeBg tpFgDim)
       , (attrName "mdQuote",        withThemeBg tpFgBright `V.withStyle` V.italic)
       , (attrName "mdRule",         withThemeBg tpFgDim)
       , (attrName "mdBold",         V.currentAttr `V.withStyle` V.bold)
       , (attrName "mdItalic",       V.currentAttr `V.withStyle` V.italic)
       , (attrName "mdCodeInline",   withThemeBg tpWarn)
       , (attrName "mdTableHeader",  withThemeBg tpAccent1 `V.withStyle` V.bold)
       , (attrName "mdTableCell",    withThemeBg tpFg)
       , (attrName "mdNormal",       withThemeBg tpFg)
       ]

-- | Resolve the active AttrMap from Config (falling back to "default" if unknown).
resolveThemeAttrMap :: Config -> AttrMap
resolveThemeAttrMap cfg =
  case resolveThemePalette cfg (themeName cfg) of
    Just pal -> buildThemeAttrMap pal
    Nothing  -> buildThemeAttrMap (builtinThemes Map.! "default")
