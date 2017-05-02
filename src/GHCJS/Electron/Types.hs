{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

module GHCJS.Electron.Types where

import           GHCJS.Types

import           Data.JSString (JSString)

import           Data.String   (IsString)

--------------------------------------------------------------------------------

-- | A constructor-less type for use in phantom type arguments.
data Any

newtype Callback (a :: k)
  = MkCallback JSVal

newtype JSArray (a :: k)
  = MkJSArray JSVal

newtype EventEmitter (a :: k)
  = MkEventEmitter JSVal

--------------------------------------------------------------------------------

newtype URL
  = MkURL JSString

newtype Path
  = MkPath JSString

--------------------------------------------------------------------------------

-- | A string representing a key combination.
newtype Accelerator
  = MkAccelerator JSString
  deriving (IsString)

newtype App
  = MkApp JSVal

newtype Bookmark
  = MkBookmark JSVal

newtype BrowserWindow
  = MkBrowserWindow JSVal

newtype BrowserWindowProxy
  = MkBrowserWindowProxy JSVal

newtype Clipboard
  = MkClipboard JSVal

newtype CommandLine
  = MkCommandLine JSVal

newtype Dialog
  = MkDialog JSVal

newtype Event
  = MkEvent JSVal

newtype GlobalShortcut
  = MkGlobalShortcut JSVal

newtype Image
  = MkImage JSVal

newtype IPCMain
  = MkIPCMain JSVal

newtype Proxy
  = MkProxy JSVal

newtype Tray
  = MkTray JSVal

--------------------------------------------------------------------------------
