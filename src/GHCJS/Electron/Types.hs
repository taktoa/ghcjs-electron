module GHCJS.Electron.Types where

import           GHCJS.Types

newtype BrowserWindowProxy
  = MkBrowserWindowProxy JSVal

newtype App
  = MkApp JSVal
