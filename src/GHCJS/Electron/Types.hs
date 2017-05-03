{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

module GHCJS.Electron.Types
  ( module GHCJS.Electron.Types
  , module Exported
  ) where

import           GHCJS.Array            as Exported
import           GHCJS.Foreign.Callback as Exported
import           GHCJS.Types            as Exported

import           Data.String            as Exported (IsString)
import           GHC.Generics           as Exported (Generic)

import           Data.JSString          (JSString)

--------------------------------------------------------------------------------

-- | A constructor-less type for use in phantom type arguments.
data Any

--------------------------------------------------------------------------------

-- FIXME: doc
data Platform
  = PlatLinux   -- FIXME: doc
  | PlatMacOS   -- FIXME: doc
  | PlatWindows -- FIXME: doc
  deriving (Eq, Generic)

-- FIXME: doc
type Linux = 'PlatLinux

-- FIXME: doc
type Win32 = 'PlatWindows

-- FIXME: doc
type Darwin = 'PlatMacOS

--------------------------------------------------------------------------------

-- FIXME: doc
newtype URL
  = MkURL JSString
  deriving (IsString, Generic)

-- FIXME: doc
newtype Path
  = MkPath JSString
  deriving (IsString, Generic)

-- | Text representing a Windows path reference.
--
--   Path references differ from plain old paths insofar as they can include
--   environment variables.
newtype WindowsPathRef
  = MkWindowsPathRef JSString
  deriving (IsString, Generic)

-- FIXME: doc
newtype HTML
  = MkHTML JSString
  deriving (IsString, Generic)

-- FIXME: doc
newtype RTF
  = MkRTF JSString
  deriving (IsString, Generic)

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
newtype Error
  = MkError JSVal

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
newtype IPCRenderer
  = MkIPCRenderer JSVal

-- FIXME: doc
newtype Proxy
  = MkProxy JSVal

-- FIXME: doc
newtype Tray
  = MkTray JSVal

--------------------------------------------------------------------------------
