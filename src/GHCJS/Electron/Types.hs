{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

module GHCJS.Electron.Types where

import           GHCJS.Types

import           Data.JSString (JSString)

import           Data.String   (IsString)

--------------------------------------------------------------------------------

-- | A constructor-less type for use in phantom type arguments.
data Any

-- FIXME: doc
newtype Callback (a :: k)
  = MkCallback JSVal

-- FIXME: doc
newtype JSArray (a :: k)
  = MkJSArray JSVal

-- FIXME: doc
newtype EventEmitter (a :: k)
  = MkEventEmitter JSVal

--------------------------------------------------------------------------------

-- FIXME: doc
newtype URL
  = MkURL JSString

-- FIXME: doc
newtype Path
  = MkPath JSString

--------------------------------------------------------------------------------

-- | A string representing a key combination.
newtype Accelerator
  = MkAccelerator JSString
  deriving (IsString)

-- FIXME: doc
newtype App
  = MkApp JSVal

-- FIXME: doc
newtype Bookmark
  = MkBookmark JSVal

-- FIXME: doc
newtype BrowserWindow
  = MkBrowserWindow JSVal

-- FIXME: doc
newtype BrowserWindowProxy
  = MkBrowserWindowProxy JSVal

-- FIXME: doc
newtype Clipboard
  = MkClipboard JSVal

-- FIXME: doc
newtype CommandLine
  = MkCommandLine JSVal

-- FIXME: doc
newtype Dialog
  = MkDialog JSVal

-- FIXME: doc
newtype Event
  = MkEvent JSVal

-- FIXME: doc
newtype GlobalShortcut
  = MkGlobalShortcut JSVal

-- FIXME: doc
newtype Image
  = MkImage JSVal

-- FIXME: doc
newtype IPCMain
  = MkIPCMain JSVal

-- FIXME: doc
newtype Proxy
  = MkProxy JSVal

-- FIXME: doc
newtype Tray
  = MkTray JSVal

--------------------------------------------------------------------------------
