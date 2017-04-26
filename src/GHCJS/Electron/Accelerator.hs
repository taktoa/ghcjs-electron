{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE LambdaCase    #-}

-- | FIXME: doc
module GHCJS.Electron.Accelerator
  ( module Exported
  , module GHCJS.Electron.Accelerator
  ) where

import           GHCJS.Electron.Types
import           GHCJS.Electron.Types as Exported ()
import           GHCJS.Types

-- | FIXME: doc
foreign import javascript safe
  "$1.globalShortcut.register($2, $3);"
  acceleratorRegister :: Electron
                      -> JSString
                      -> Callback ()
                      -> IO ()

-- | Represents a particular subset of the set of all key combinations.
newtype KeyQuery
  = MkKeyQuery
    { _keyCombinations :: [KeyCombination] }
  deriving (Eq, Show, Read, Generic, Monoid)

simpleKQ :: [KeyModifier] -> KeyCode -> KeyQuery
simpleKQ modifier key = MkKeyQuery [MkKeyCombination key modifier]

-- | A combination of a key and a modifier.
data KeyCombination
  = MkKeyCombination
    { _kcCode      :: {-# UNPACK #-} !KeyCode
    , _kcModifiers :: {-# UNPACK #-} ![KeyModifier]
    }
  deriving (Eq, Show, Read, Generic)

-- | A key modifier.
data KeyModifier
  = KeyModCommand
  | KeyModControl
  | KeyModAlt
  | KeyModAltGr
  | KeyModShift
  | KeyModSuper
    -- ^ This is the "Windows key" on Linux/Windows keyboards.
    --   On Mac keyboards, it is "Cmd".
  deriving (Eq, Show, Read, Generic)

-- | FIXME: doc
keyModToAccelerator :: KeyModifier -> Text
keyModToAccelerator KeyModCommand = "Command"
keyModToAccelerator KeyModControl = "Control"
keyModToAccelerator KeyModAlt     = "Alt"
keyModToAccelerator KeyModAltGr   = "AltGr"
keyModToAccelerator KeyModShift   = "Shift"
keyModToAccelerator KeyModSuper   = "Super"

-- | A key code.
data KeyCode
  = KeyCodeLetter     {-# UNPACK #-} !KeyCodeLetter
  | KeyCodeDigit      {-# UNPACK #-} !KeyCodeDigit
  | KeyCodeSymbol     {-# UNPACK #-} !KeyCodeSymbol
  | KeyCodeFunction   {-# UNPACK #-} !KeyCodeFunction
  | KeyCodeNavigation {-# UNPACK #-} !KeyCodeNavigation
  | KeyCodeSpecial    {-# UNPACK #-} !KeyCodeSpecial
  | KeyCodeMisc       {-# UNPACK #-} !KeyCodeMisc
  deriving (Eq, Show, Read, Generic)

-- | FIXME: doc
keyCodeToAccelerator :: KeyCode -> Text
keyCodeToAccelerator = go >>> fromMaybe (error "This should not happen.")
  where
    go :: KeyCode -> Maybe Text
    go (KeyCodeLetter     kcl) = goLetter     kcl
    go (KeyCodeDigit      kcd) = goDigit      kcd
    go (KeyCodeSymbol     kcs) = goSymbol     kcs
    go (KeyCodeFunction   kcf) = goFunction   kcf
    go (KeyCodeNavigation kcn) = goNavigation kcn
    go (KeyCodeSpecial    kcs) = goSpecial    kcs
    go (KeyCodeMisc       kcm) = goMisc       kcm

    goLetter     :: KeyCodeLetter     -> Maybe Text
    goDigit      :: KeyCodeDigit      -> Maybe Text
    goSymbol     :: KeyCodeSymbol     -> Maybe Text
    goFunction   :: KeyCodeFunction   -> Maybe Text
    goNavigation :: KeyCodeNavigation -> Maybe Text
    goSpecial    :: KeyCodeSpecial    -> Maybe Text
    goMisc       :: KeyCodeMisc       -> Maybe Text

    goLetter     = stripKey >=> checkLen 1 1
    goDigit      = stripKey >=> checkLen 1 1
    goSymbol     = \case KeyBacktick  -> pure "`"
                         KeySemicolon -> pure ";"
                         KeyQuote     -> pure "'"
                         KeyOBracket  -> pure "["
                         KeyCBracket  -> pure "]"
                         KeyBackslash -> pure "\\"
                         KeySlash     -> pure "/"
                         KeyPeriod    -> pure "."
                         KeyComma     -> pure ","
                         KeyHyphen    -> pure "-"
                         KeyEquals    -> pure "="
    goFunction   = stripKey >=> \key -> do guard (T.all isDigit key)
                                           checkLen 1 2 key
    goNavigation = stripKey >=> checkLen 0 10
    goSpecial    = \case KeyMediaNext  -> pure "MediaNextTrack"
                         KeyMediaPrev  -> pure "MediaPreviousTrack"
                         KeyMediaStop  -> pure "MediaStop"
                         KeyMediaPlay  -> pure "MediaPlayPause"
                         KeyVolumeUp   -> pure "VolumeUp"
                         KeyVolumeDown -> pure "VolumeDown"
                         KeyVolumeMute -> pure "VolumeMute"
    goMisc       = stripKey >=> checkLen 0 12

    checkLen :: (Int, Int) -> Text -> Maybe Text
    checkLen minLen maxLen t = do
      guard (T.length t >= minLen)
      guard (T.length t <= maxLen)
      pure t

    stripKey :: (Show s) => s -> Maybe Text
    stripKey x = do let shown = T.pack (show x)
                    guard (T.take 3 shown == "Key")
                    pure $ T.drop 3 shown

-- | Key codes that represent letters.
data KeyCodeLetter
  = KeyA | KeyB | KeyC | KeyD | KeyE | KeyF | KeyG | KeyH | KeyI | KeyJ
  | KeyK | KeyL | KeyM | KeyN | KeyO | KeyP | KeyQ | KeyR | KeyS | KeyT
  | KeyU | KeyV | KeyW | KeyX | KeyY | KeyZ
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

-- | Key codes that represent digits.
data KeyCodeDigit
  = Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

-- | Key codes that represent symbols.
data KeyCodeSymbol
  = KeyBacktick  -- ^ The backtick/tilde key.
  | KeySemicolon -- ^ The semicolon/colon key.
  | KeyQuote     -- ^ The single/double quote key.
  | KeyOBracket  -- ^ The open-square/curly bracket key.
  | KeyCBracket  -- ^ The close-square/curly bracket key.
  | KeyBackslash -- ^ The backslash/pipe key.
  | KeySlash     -- ^ The slash/question mark key.
  | KeyPeriod    -- ^ The period/greater-than key.
  | KeyComma     -- ^ The comma/less-than key.
  | KeyHyphen    -- ^ The hyphen/underscore key.
  | KeyEquals    -- ^ The equals/plus key.
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

-- | Key codes corresponding to the F1-F24 keys ("function" keys).
data KeyCodeFunction
  = KeyF1  | KeyF2  | KeyF3  | KeyF4  | KeyF5  | KeyF6
  | KeyF7  | KeyF8  | KeyF9  | KeyF10 | KeyF11 | KeyF12
  | KeyF13 | KeyF14 | KeyF15 | KeyF16 | KeyF17 | KeyF18
  | KeyF19 | KeyF20 | KeyF21 | KeyF22 | KeyF23 | KeyF24
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

-- | Key codes corresponding to the arrow keys, page up/down, home, and end.
data KeyCodeNavigation
  = KeyUp | KeyDown | KeyLeft | KeyRight
  | KeyPageUp | KeyPageDown | KeyHome | KeyEnd
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

-- | Key codes corresponding to special keys, like @XF86AudioPlayPause@.
data KeyCodeSpecial
  = KeyMediaNextTrack
  | KeyMediaPrevTrack
  | KeyMediaStop
  | KeyMediaPlayPause
  | KeyVolumeUp
  | KeyVolumeDown
  | KeyVolumeMute
  deriving (Eq, Enum, Bounded, Show, Read, Generic)

-- | Other various key codes.
data KeyCodeMisc
  = KeySpace | KeyTab | KeyBackspace | KeyDelete | KeyEnter
  | KeyInsert | KeyEscape | KeyPrintScreen
  deriving (Eq, Enum, Bounded, Show, Read, Generic)
