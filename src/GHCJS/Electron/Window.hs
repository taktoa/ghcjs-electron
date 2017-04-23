{-# LANGUAGE JavaScriptFFI #-}

module GHCJS.Electron.Window where

import           GHCJS.Electron.Types
import           GHCJS.Types

foreign import javascript safe
  "$r = window.open($1);"
  open :: JSString -- ^ URL
       -> IO BrowserWindowProxy

foreign import javascript safe
  "$r = window.open($1, $2, $3);"
  openFull :: JSString -- ^ URL
           -> JSString -- ^ Frame name
           -> JSString -- ^ Features
           -> IO BrowserWindowProxy
