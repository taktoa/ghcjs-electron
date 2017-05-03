{-# LANGUAGE JavaScriptFFI #-}

-- | A wrapper over the Electron power-save blocking API, as documented
--   <https://electron.atom.io/docs/api/power-save-blocker here>.
module GHCJS.Electron.PowerSaveBlocker
  ( PowerSaveBlocker (..)
  , BlockerID (..)
  , unsafeGetPowerSaveBlocker
  , unsafeStart
  , unsafeStop
  , unsafeIsStarted
  ) where

import           GHCJS.Types

-- | An Electron @powerSaveBlocker@ object.
newtype PowerSaveBlocker
  = MkPowerSaveBlocker JSVal

-- | A power save blocker ID number.
newtype BlockerID
  = MkBlockerID Int

-- | Get the canonical 'PowerSaveBlocker' object, i.e.: the value of
--   @require('electron').powerSaveBlocker@.
foreign import javascript safe
  "$r = require('electron').powerSaveBlocker;"
  unsafeGetPowerSaveBlocker :: IO PowerSaveBlocker

-- | Start a power save blocker of the given type.
--
--   Returns a power save blocker ID (which is an integer).
--
--   The type parameter is one of the following strings:
--
--   * @"prevent-app-suspension"@
--       * Prevents the application from being suspended.
--       * Keeps the system active but allows the screen to be turned off.
--       * Example use cases: downloading a file or playing audio.
--   * @"prevent-display-sleep"@
--       * Prevents the display from going to sleep.
--       * Keeps the system and screen active.
--       * Example use case: playing video.
--       * Has higher precedence than @"prevent-app-suspension"@.
--
--   Only the highest precedence type will have any effect.
foreign import javascript safe
  "$r = $1.start($2);"
  unsafeStart :: PowerSaveBlocker
              -> JSString
              -- ^ The power save blocker type.
              -> IO BlockerID

-- | Stops the specified power save blocker by its 'BlockerID'.
foreign import javascript safe
  "$1.stop($2);"
  unsafeStop :: PowerSaveBlocker
             -> BlockerID
             -> IO ()

-- | Checks whether the given 'BlockerID' corresponds to a running power save
--   blocker process.
foreign import javascript safe
  "$r = $1.isStarted($2);"
  unsafeIsStarted :: PowerSaveBlocker
                  -> BlockerID
                  -> IO Bool
