{-# LANGUAGE JavaScriptFFI #-}

-- | FIXME: doc
module GHCJS.Electron.IPCMain
  ( module Exported
  , module GHCJS.Electron.IPCMain
  ) where

import           GHCJS.Electron.Types
import           GHCJS.Electron.Types as Exported ()
import           GHCJS.Types

-- | Get the global 'IPCMain' object.
foreign import javascript safe
  "$r = require('electron').ipcMain;"
  getIPCMain :: IO IPCMain

-- | Listen for the given event and run the given callback whenever it occurs.
foreign import javascript safe
  "$1.on($2, $3);"
  ipcMainListenerOn :: IPCMain
                    -> JSString
                    -> Callback (Event -> Any -> ())
                    -> IO ()

-- | Listen for the given event and run the given callback exactly once;
--   i.e.: the callback will be run precisely the first time the event occurs
--   after this function is run.
foreign import javascript safe
  "$1.once($2, $3);"
  ipcMainListenerOnce :: IPCMain
                      -> JSString
                      -> Callback (Event -> Any -> ())
                      -> IO ()

-- | Remove all listeners on the given 'IPCMain'.
foreign import javascript safe
  "$1.removeAllListeners();"
  ipcMainRemoveAllListeners :: IPCMain -> IO ()

-- | Remove all listeners for the given event on the given 'IPCMain'.
foreign import javascript safe
  "$1.removeAllListeners($2);"
  ipcMainRemoveAllListenersOnEvent :: IPCMain -> JSString -> IO ()
