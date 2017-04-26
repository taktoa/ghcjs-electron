{-# LANGUAGE JavaScriptFFI #-}

module GHCJS.Electron.Window where

import           GHCJS.Electron.Types
import           GHCJS.Types

-- | Open a new window with the URL set to the given string.
foreign import javascript safe
  "$r = window.open($1);"
  open :: JSString -- ^ URL
       -> IO BrowserWindowProxy

-- | Open a new window with the given URL, frame name, and features string.
foreign import javascript safe
  "$r = window.open($1, $2, $3);"
  openFull :: JSString -- ^ URL
           -> JSString -- ^ Frame name
           -> JSString -- ^ Features
           -> IO BrowserWindowProxy
