{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A wrapper over the Electron renderer IPC API, as documented
--   <https://electron.atom.io/docs/api/ipc-renderer here>.
module GHCJS.Electron.IPCRenderer
  ( module GHCJS.Electron.IPCRenderer -- FIXME: specific export list
  ) where

import           GHCJS.Node.EventEmitter

import           GHCJS.Electron.Types

import           GHCJS.Array

import           JavaScript.JSON.Types   (Value)

-- | Get the canonical 'IPCRenderer' object, i.e.: the value of
--   @require('electron').ipcRenderer@.
foreign import javascript safe
  "$r = require('electron').ipcRenderer;"
  unsafeGetIPCRenderer :: IO IPCRenderer

data IPCEvent -- FIXME: implement

instance IsEventEmitter IPCEvent IPCRenderer where
  toEventEmitter (MkIPCRenderer val) = pure (MkEventEmitter val)

-- | Send a message to the main process asynchronously via a named channel.
--   The message comes with an arbitrary array of arguments.
foreign import javascript safe
  "(function(obj) { obj.send.apply(obj, [$2] + $3); })($1);"
  unsafeSend :: IPCRenderer
             -> Channel
             -- ^ The channel on which a message will be asynchronously sent
             --   to the main process.
             -> Array Value
             -- ^ The data associated with the message.
             -> IO ()

-- | Send a message to the main process synchronously via a named channel.
--   The message comes with an arbitrary array of arguments.
--
--   The main process handles it by listening for channel with @ipcMain@ module,
--   and replies by setting @event.returnValue@.
--
--   Note: Sending a synchronous message will block the whole renderer process,
--   unless you know what you are doing you should probably never use this
--   function.
foreign import javascript safe
  "$r = (function(obj) { obj.sendSync.apply(obj, [$2] + $3); })($1);"
  unsafeSendSync :: IPCRenderer
                 -> Channel
                 -- ^ The channel on which a message will be synchronously sent
                 --   to the main process.
                 -> Array Value
                 -- ^ The data associated with the message.
                 -> IO Value
                 -- ^ The returned value.
