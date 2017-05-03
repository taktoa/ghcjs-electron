{-# LANGUAGE JavaScriptFFI #-}

-- | A wrapper over the Electron debugging API, as documented
--   <https://electron.atom.io/docs/api/debugger here>.
--
--   Up-to-date documentation on the Chrome DevTools Protocol can be seen
--   <https://chromedevtools.github.io/devtools-protocol here>.
module GHCJS.Electron.Debugger
  ( Debugger (..)
  , unsafeGetDebugger
  , unsafeAttach
  , unsafeAttachWithVersion
  , unsafeDetach
  , unsafeSendCommand
  ) where

import           Data.Text            (Text)

import           GHCJS.Types

import           GHCJS.Electron.Types

import           JavaScript.Object

-- | An Electron @debugger@ object.
newtype Debugger
  = MkDebugger JSVal

-- data DebuggerEvent
--   = DebuggerDetach
--     { event  :: !Event
--     , reason :: !Text
--     }
--   | DebuggerMessage
--     { event  :: !Event
--     , method :: !Text
--     , params :: !Object
--     }

-- | Get the 'Debugger' object for a given 'BrowserWindow'.
foreign import javascript safe
  "$r = $1.webContents.debugger;"
  unsafeGetDebugger :: BrowserWindow -> IO Debugger

-- | Attaches the given 'Debugger' to the @webContents@.
foreign import javascript safe
  "$1.attach();"
  unsafeAttach :: Debugger -> IO ()

-- | Attaches the given 'Debugger' to the @webContents@.
foreign import javascript safe
  "$1.attach($2);"
  unsafeAttachWithVersion :: Debugger
                          -> JSString
                          -- ^ The requested debugging protocol version.
                          -> IO ()

-- | Detaches the given 'Debugger' from the @webContents@.
foreign import javascript safe
  "$1.detach();"
  unsafeDetach :: Debugger -> IO ()

-- | Send the given command to the debugging target.
--
--   Up-to-date documentation on the Chrome DevTools Protocol can be seen
--   <https://chromedevtools.github.io/devtools-protocol here>.
foreign import javascript safe
  "$1.sendCommand($2, $3, $4);"
  unsafeSendCommand :: Debugger
                    -> JSString
                    -- ^ The method name (as defined in the Chrome DevTools
                    --   Protocol) to call.
                    -> Object
                    -- ^ A JSON object containing request parameters.
                    -> Callback (JSVal -> JSVal -> IO ())
                    -- ^ The callback that will be run when the method returns.
                    --
                    --   The first parameter of the callback contains any
                    --   relevant error information, and the second parameter
                    --   contains any returned data.
                    -> IO ()
