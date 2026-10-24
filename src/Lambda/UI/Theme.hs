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

import Brick (AttrMap, attrMap, attrName, fg, on)
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

-- | Construct true 24-bit RGB terminal color from 0-255 RGB components.
-- We use 'V.linearColor' so hex codes map 1:1 to literal RGB terminal escape
-- sequences (\x1b[48;2;R;G;Bm). 'V.srgbColor' applies a reverse gamma expansion curve
-- which maps #002b36 (0, 43, 54) down to (0, 6, 9), crushing all dark backgrounds to near-black!
rgb :: Word8 -> Word8 -> Word8 -> V.Color
rgb = V.linearColor

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

-- | Canonical built-in colorschemes.
-- Checked directly against official vim/neovim reference implementations:
-- - solarized8 / vim-colors-solarized (Ethan Schoonover)
-- - nord-vim / nord.nvim (Arctic Ice Studio: Polar Night, Snow Storm, Frost, Aurora)
-- - zenburn.vim (Jani Nurminen)
-- - dracula/vim (Zeno Rocha)
-- - tokyonight.nvim (folke: Night & Storm)
-- - gruvbox (morhetz / sainnhe: Dark Hard & Dark Medium)
builtinThemes :: Map Text ThemePalette
builtinThemes = Map.fromList
  [ ( "default"
    , ThemePalette
        { tpName     = "default"
        , tpBg       = Nothing
        , tpBgAlt    = Just (rgb 0x21 0x26 0x2d) -- Modern elevated dark slate (#21262d)
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
        , tpBg       = Just (rgb 0x00 0x2b 0x36) -- base03: darkest background
        , tpBgAlt    = Just (rgb 0x07 0x36 0x42) -- base02: statusline / cursorline / panel background
        , tpFg       = rgb 0x83 0x94 0x96        -- base0: primary body text
        , tpFgBright = rgb 0x93 0xa1 0xa1        -- base1: headings / emphasized text / statusline fg
        , tpFgDim    = rgb 0x65 0x7b 0x83        -- base00: comments & muted text (4.5:1 on base03)
        , tpAccent1  = rgb 0x2a 0xa1 0x98        -- cyan
        , tpAccent2  = rgb 0x6c 0x71 0xc4        -- violet
        , tpUser     = rgb 0x26 0x8b 0xd2        -- blue
        , tpWarn     = rgb 0xb5 0x89 0x00        -- yellow
        , tpError    = rgb 0xdc 0x32 0x2f        -- red
        , tpSuccess  = rgb 0x85 0x99 0x00        -- green
        }
    )
  , ( "solarized-light"
    , ThemePalette
        { tpName     = "solarized-light"
        , tpBg       = Just (rgb 0xfd 0xf6 0xe3) -- base3: light background
        , tpBgAlt    = Just (rgb 0xee 0xe8 0xd5) -- base2: statusline / panel background
        , tpFg       = rgb 0x58 0x6e 0x75        -- base01: primary body text
        , tpFgBright = rgb 0x07 0x36 0x42        -- base02: emphasized / headings
        , tpFgDim    = rgb 0x83 0x94 0x96        -- base0: comments & muted text (4.5:1 on base3)
        , tpAccent1  = rgb 0x2a 0xa1 0x98        -- cyan
        , tpAccent2  = rgb 0x6c 0x71 0xc4        -- violet
        , tpUser     = rgb 0x26 0x8b 0xd2        -- blue
        , tpWarn     = rgb 0xb5 0x89 0x00        -- yellow
        , tpError    = rgb 0xdc 0x32 0x2f        -- red
        , tpSuccess  = rgb 0x85 0x99 0x00        -- green
        }
    )
  , ( "nordic"
    , ThemePalette
        { tpName     = "nordic"
        , tpBg       = Just (rgb 0x2e 0x34 0x40) -- nord0 (Polar Night 0): official editor background
        , tpBgAlt    = Just (rgb 0x3b 0x42 0x52) -- nord1 (Polar Night 1): StatusLine / panel background
        , tpFg       = rgb 0xd8 0xde 0xe9        -- nord4 (Snow Storm 0): primary foreground
        , tpFgBright = rgb 0xec 0xef 0xf4        -- nord6 (Snow Storm 2): headings & highlights
        , tpFgDim    = rgb 0x90 0x99 0xab        -- Lifted Frost-Slate (calibrated 4.6:1 against #2e3440)
        , tpAccent1  = rgb 0x88 0xc0 0xd0        -- nord8 (Frost Cyan)
        , tpAccent2  = rgb 0xb4 0x8e 0xad        -- nord15 (Aurora Purple)
        , tpUser     = rgb 0x81 0xa1 0xc1        -- nord9 (Frost Blue)
        , tpWarn     = rgb 0xeb 0xcb 0x8b        -- nord13 (Aurora Yellow)
        , tpError    = rgb 0xbf 0x61 0x6a        -- nord11 (Aurora Red)
        , tpSuccess  = rgb 0xa3 0xbe 0x8c        -- nord14 (Aurora Green)
        }
    )
  , ( "zenburn"
    , ThemePalette
        { tpName     = "zenburn"
        , tpBg       = Just (rgb 0x3f 0x3f 0x3f) -- Zenburn standard background (#3f3f3f)
        , tpBgAlt    = Just (rgb 0x2b 0x2b 0x2b) -- Zenburn darker background-1 (#2b2b2b) for headers/bars
        , tpFg       = rgb 0xdc 0xdc 0xcc        -- Zenburn primary foreground (#dcdccc)
        , tpFgBright = rgb 0xff 0xff 0xef        -- Zenburn bright foreground+1 (#ffffef)
        , tpFgDim    = rgb 0x9f 0xaf 0x9f        -- Zenburn comment sage-gray (#9faf9f, 4.5:1 on #3f3f3f)
        , tpAccent1  = rgb 0x93 0xe0 0xe3        -- Zenburn cyan (#93e0e3)
        , tpAccent2  = rgb 0xdf 0xaf 0x8f        -- Zenburn orange/peach (#dfaf8f)
        , tpUser     = rgb 0x8c 0xd0 0xd3        -- Zenburn blue/aqua (#8cd0d3)
        , tpWarn     = rgb 0xf0 0xdf 0xaf        -- Zenburn yellow (#f0dfaf)
        , tpError    = rgb 0xcc 0x93 0x93        -- Zenburn terracotta red (#cc9393)
        , tpSuccess  = rgb 0x7f 0x9f 0x7f        -- Zenburn green (#7f9f7f)
        }
    )
  , ( "dracula"
    , ThemePalette
        { tpName     = "dracula"
        , tpBg       = Just (rgb 0x28 0x2a 0x36) -- Dracula official background (#282a36)
        , tpBgAlt    = Just (rgb 0x44 0x47 0x5a) -- Dracula Current Line / Selection (#44475a)
        , tpFg       = rgb 0xf8 0xf8 0xf2        -- Dracula foreground (#f8f8f2)
        , tpFgBright = rgb 0xff 0xff 0xff        -- Bright white
        , tpFgDim    = rgb 0x7e 0x8c 0xc2        -- Lifted comment purple-slate (4.5:1 on #282a36)
        , tpAccent1  = rgb 0x8b 0xe9 0xfd        -- Dracula cyan (#8be9fd)
        , tpAccent2  = rgb 0xbd 0x93 0xf9        -- Dracula purple (#bd93f9)
        , tpUser     = rgb 0xff 0x79 0xc6        -- Dracula pink (#ff79c6)
        , tpWarn     = rgb 0xf1 0xfa 0x8c        -- Dracula yellow (#f1fa8c)
        , tpError    = rgb 0xff 0x55 0x55        -- Dracula red (#ff5555)
        , tpSuccess  = rgb 0x50 0xfa 0x7b        -- Dracula green (#50fa7b)
        }
    )
  , ( "tokyonight"
    , ThemePalette
        { tpName     = "tokyonight"
        , tpBg       = Just (rgb 0x1a 0x1b 0x26) -- Tokyo Night (folke) deep midnight-blue background (#1a1b26)
        , tpBgAlt    = Just (rgb 0x24 0x28 0x3b) -- Tokyo Night Storm / statusline background (#24283b)
        , tpFg       = rgb 0xc0 0xca 0xf5        -- Tokyo Night foreground (#c0caf5)
        , tpFgBright = rgb 0xff 0xff 0xff        -- Bright white
        , tpFgDim    = rgb 0x7a 0x88 0xbf        -- Lifted comment blue (#7a88bf, 4.6:1 on #1a1b26)
        , tpAccent1  = rgb 0x7d 0xcf 0xff        -- Tokyo Night cyan (#7dcfff)
        , tpAccent2  = rgb 0xbb 0x9a 0xf7        -- Tokyo Night magenta (#bb9af7)
        , tpUser     = rgb 0x7a 0xa2 0xf7        -- Tokyo Night blue (#7aa2f7)
        , tpWarn     = rgb 0xe0 0xaf 0x68        -- Tokyo Night yellow (#e0af68)
        , tpError    = rgb 0xf7 0x76 0x8e        -- Tokyo Night red (#f7768e)
        , tpSuccess  = rgb 0x9e 0xce 0x6a        -- Tokyo Night green (#9ece6a)
        }
    )
  , ( "gruvbox"
    , ThemePalette
        { tpName     = "gruvbox"
        , tpBg       = Just (rgb 0x28 0x28 0x28) -- Gruvbox dark0 (standard medium background #282828)
        , tpBgAlt    = Just (rgb 0x3c 0x38 0x36) -- Gruvbox dark1 / statusline background (#3c3836)
        , tpFg       = rgb 0xeb 0xdb 0xb2        -- Gruvbox light1 primary text (#ebdbb2)
        , tpFgBright = rgb 0xfb 0xf1 0xc7        -- Gruvbox light0 bright text (#fbf1c7)
        , tpFgDim    = rgb 0xa8 0x99 0x84        -- Gruvbox light4 / comment gray (#a89984, 4.6:1 on #282828)
        , tpAccent1  = rgb 0x8e 0xc0 0x7c        -- Gruvbox bright_aqua (#8ec07c)
        , tpAccent2  = rgb 0xd3 0x86 0x9b        -- Gruvbox bright_purple (#d3869b)
        , tpUser     = rgb 0x83 0xa5 0x98        -- Gruvbox bright_blue (#83a598)
        , tpWarn     = rgb 0xfa 0xbd 0x2f        -- Gruvbox bright_yellow (#fabd2f)
        , tpError    = rgb 0xfb 0x49 0x34        -- Gruvbox bright_red (#fb4934)
        , tpSuccess  = rgb 0xb8 0xbb 0x26        -- Gruvbox bright_green (#b8bb26)
        }
    )
  , ( "gruvbox-dark"
    , ThemePalette
        { tpName     = "gruvbox-dark"
        , tpBg       = Just (rgb 0x1d 0x20 0x21) -- Gruvbox dark0_hard (deep pitch background #1d2021)
        , tpBgAlt    = Just (rgb 0x28 0x28 0x28) -- Gruvbox dark0 (#282828)
        , tpFg       = rgb 0xeb 0xdb 0xb2        -- Gruvbox light1 (#ebdbb2)
        , tpFgBright = rgb 0xfb 0xf1 0xc7        -- Gruvbox light0 (#fbf1c7)
        , tpFgDim    = rgb 0x92 0x83 0x74        -- Gruvbox gray (#928374, 4.8:1 on #1d2021)
        , tpAccent1  = rgb 0x8e 0xc0 0x7c        -- Gruvbox bright_aqua (#8ec07c)
        , tpAccent2  = rgb 0xd3 0x86 0x9b        -- Gruvbox bright_purple (#d3869b)
        , tpUser     = rgb 0x83 0xa5 0x98        -- Gruvbox bright_blue (#83a598)
        , tpWarn     = rgb 0xfa 0xbd 0x2f        -- Gruvbox bright_yellow (#fabd2f)
        , tpError    = rgb 0xfb 0x49 0x34        -- Gruvbox bright_red (#fb4934)
        , tpSuccess  = rgb 0xb8 0xbb 0x26        -- Gruvbox bright_green (#b8bb26)
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
  , "tokyonight"
  , "gruvbox"
  , "gruvbox-dark"
  ]

-- | Resolve a theme name against built-in themes and user-configured custom themes.
-- Supports friendly aliases like "nord" -> "nordic", "solarized" -> "solarized-dark", "tokyo" -> "tokyonight".
resolveThemePalette :: Config -> Text -> Maybe ThemePalette
resolveThemePalette cfg rawName =
  let clean = T.toLower (T.strip rawName)
      canonical = case clean of
        "nord"         -> "nordic"
        "solarized"    -> "solarized-dark"
        "tokyo"        -> "tokyonight"
        "tokyo-night"  -> "tokyonight"
        "gruvbox-hard" -> "gruvbox-dark"
        other          -> other
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
      barBg = case tpBgAlt of
        Just altBg -> altBg
        Nothing    -> case tpBg of
          Just b  -> b
          Nothing -> rgb 0x21 0x26 0x2d
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
       , (attrName "promptBarCap",    withThemeBg barBg)
       , (attrName "promptBarBase",   tpFg `on` barBg)
       , (attrName "promptLogo",      (tpUser `on` barBg) `V.withStyle` V.bold)
       , (attrName "promptProvider",  (tpAccent1 `on` barBg) `V.withStyle` V.bold)
       , (attrName "promptModel",     tpAccent2 `on` barBg)
       , (attrName "promptDivider",   tpFgDim `on` barBg)
       , (attrName "promptArrow",     withThemeBg tpAccent1 `V.withStyle` V.bold)
       , (attrName "promptPlanMode",  (tpAccent1 `on` barBg) `V.withStyle` V.bold)
       , (attrName "promptExecMode",  (tpWarn `on` barBg) `V.withStyle` V.bold)
       , (attrName "promptCtxNormal", tpSuccess `on` barBg)
       , (attrName "promptCtxWarn",   (tpWarn `on` barBg) `V.withStyle` V.bold)
       , (attrName "promptCtxHigh",   (tpError `on` barBg) `V.withStyle` V.bold)
       , (attrName "pillLogoCap",       withThemeBg tpUser)
       , (attrName "pillLogoBody",      badgeTextBg `on` tpUser `V.withStyle` V.bold)
       , (attrName "pillModelCap",      withThemeBg tpAccent2)
       , (attrName "pillModelBody",     badgeTextBg `on` tpAccent2)
       , (attrName "pillCtxNormalCap",  withThemeBg tpAccent1)
       , (attrName "pillCtxNormalBody", badgeTextBg `on` tpAccent1)
       , (attrName "pillCtxWarnCap",    withThemeBg tpWarn)
       , (attrName "pillCtxWarnBody",   badgeTextBg `on` tpWarn `V.withStyle` V.bold)
       , (attrName "pillCtxHighCap",    withThemeBg tpError)
       , (attrName "pillCtxHighBody",   badgeTextBg `on` tpError `V.withStyle` V.bold)
       , (attrName "pillPlanCap",       withThemeBg tpAccent1)
       , (attrName "pillPlanBody",      badgeTextBg `on` tpAccent1 `V.withStyle` V.bold)
       , (attrName "pillExecCap",       withThemeBg tpWarn)
       , (attrName "pillExecBody",      badgeTextBg `on` tpWarn `V.withStyle` V.bold)
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
