{-# LANGUAGE JavaScriptFFI #-}

-- FIXME: doc
module GHCJS.Electron.GlobalShortcut where

import           GHCJS.Electron.Types

-- | Get the canonical 'GlobalShortcut' object, i.e.: the value of
--   @require('electron').globalShortcut@.
foreign import javascript safe
  "$r = require('electron').globalShortcut;"
  getGlobalShortcut :: IO GlobalShortcut

-- | Register a callback for the given 'Accelerator'.
foreign import javascript safe
  "$1.register($2, $3);"
  register :: GlobalShortcut
           -> Accelerator
           -> Callback ()
           -> IO ()

-- | Deregister any callbacks registered to the given 'Accelerator'.
foreign import javascript safe
  "$1.unregister($2);"
  unregister :: GlobalShortcut
             -> Accelerator
             -> IO ()

-- | Deregister all global shortcut callbacks.
foreign import javascript safe
  "$1.unregisterAll();"
  unregisterAll :: GlobalShortcut
                -> IO ()

-- | Check if the given 'Accelerator' is has been registered.
foreign import javascript safe
  "$1.isRegistered($2);"
  isRegistered :: GlobalShortcut
               -> Accelerator
               -> IO Bool
