module GHCJS.Electron.Types where

import           GHCJS.Types

--------------------------------------------------------------------------------

-- | A constructor-less type for use in phantom type arguments.
data Any

newtype Callback a
  = MkCallback JSVal

newtype JSArray a
  = MkJSArray JSVal

--------------------------------------------------------------------------------

newtype URL
  = MkURL JSString

newtype Path
  = MkPath JSString

--------------------------------------------------------------------------------

newtype App
  = MkApp JSVal

newtype BrowserWindow
  = MkBrowserWindow JSVal

newtype BrowserWindowProxy
  = MkBrowserWindowProxy JSVal

newtype CommandLine
  = MkCommandLine JSVal

newtype Proxy
  = MkProxy JSVal

--------------------------------------------------------------------------------
