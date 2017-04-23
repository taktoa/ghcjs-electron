{-# LANGUAGE JavaScriptFFI #-}
-- | The BrowserWindowProxy object is returned from window.open and provides
--   limited functionality with the child window.
module GHCJS.Electron.BrowserWindowProxy
  ( module Exported
  , module GHCJS.Electron.BrowserWindowProxy
  ) where

import           GHCJS.Electron.Types as Exported (BrowserWindowProxy)
import           GHCJS.Types

-- | Removes focus from the child window.
foreign import javascript safe
  "$1.blur();"
  browserBlur :: BrowserWindowProxy
              -> IO ()

-- | Forcefully closes the child window without calling its unload event.
foreign import javascript safe
  "$1.close();"
  browserClose :: BrowserWindowProxy
               -> IO ()

-- | Evaluates the code in the child window.
foreign import javascript safe
  "$1.eval($2);"
  browserEval :: BrowserWindowProxy
              -> JSString
              -- ^ code
              -> IO ()

-- | Focuses the child window (brings the window to front).
foreign import javascript safe
  "$1.focus();"
  browserFocus :: BrowserWindowProxy
               -> IO ()

-- | Invokes the print dialog on the child window.
foreign import javascript safe
  "$1.print();"
  browserPrint :: BrowserWindowProxy
               -> IO ()

-- | Send a message to the child window with the specified origin or use "*"
--   for no origin preference.
foreign import javascript safe
  "$1.postMessage($2, $3);"
  browserPostMessage :: BrowserWindowProxy
                     -> JSString
                     -- ^ message
                     -> JSString
                     -- ^ targetOrigin
                     -> IO ()

-- | Check if the child window has been closed.
foreign import javascript safe
  "$r = $1.closed;"
  browserIsClosed :: BrowserWindowProxy
                  -> IO Bool
