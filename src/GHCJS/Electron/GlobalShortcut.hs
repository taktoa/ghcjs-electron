{-# LANGUAGE JavaScriptFFI #-}

-- | A wrapper over the Electron global shortcut API, as documented
--   <https://electron.atom.io/docs/api/global-shortcut here>.
module GHCJS.Electron.GlobalShortcut
  ( module Exported
  , unsafeGetGlobalShortcut
  , unsafeRegister
  , unsafeUnregister
  , unsafeUnregisterAll
  , unsafeIsRegistered
  ) where

import           GHCJS.Electron.Types

import           GHCJS.Electron.Types as Exported (GlobalShortcut (..))

-- | Get the canonical 'GlobalShortcut' object, i.e.: the value of
--   @require('electron').globalShortcut@.
foreign import javascript safe
  "$r = require('electron').globalShortcut;"
  unsafeGetGlobalShortcut :: IO GlobalShortcut

-- | Register a callback for the given 'Accelerator'.
foreign import javascript safe
  "$1.register($2, $3);"
  unsafeRegister :: GlobalShortcut
                 -> Accelerator
                 -> Callback ()
                 -> IO ()

-- | Deregister any callbacks registered to the given 'Accelerator'.
foreign import javascript safe
  "$1.unregister($2);"
  unsafeUnregister :: GlobalShortcut
                   -> Accelerator
                   -> IO ()

-- | Deregister all global shortcut callbacks.
foreign import javascript safe
  "$1.unregisterAll();"
  unsafeUnregisterAll :: GlobalShortcut
                      -> IO ()

-- | Check if the given 'Accelerator' is has been registered.
foreign import javascript safe
  "$1.isRegistered($2);"
  unsafeIsRegistered :: GlobalShortcut
                     -> Accelerator
                     -> IO Bool
