{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                     (void)

import           Data.JSString

import qualified GHCJS.Electron.Accelerator        as Accelerator
import qualified GHCJS.Electron.BrowserWindowProxy as BrowserWindowProxy
import qualified GHCJS.Electron.Clipboard          as Clipboard
import qualified GHCJS.Electron.GlobalShortcut     as GlobalShortcut
import qualified GHCJS.Electron.Shell              as Shell
import qualified GHCJS.Electron.Tray               as Tray
import qualified GHCJS.Electron.Window             as Window

testWindow :: IO ()
testWindow = void $ do
  bwp <- Window.open "https://google.com"
  BrowserWindowProxy.browserBlur bwp
  BrowserWindowProxy.browserEval bwp "alert(\"DEBUG!\");"
  BrowserWindowProxy.browserClose bwp

-- testTray :: IO ()
-- testTray = void $ do
--   Tray.trayNew ""

testClipboard :: IO ()
testClipboard = void $ do
  cb <- Clipboard.unsafeGetClipboard "clipboard"
  -- Clipboard.unsafeAvailableFormats cb >>= print
  Clipboard.unsafeWriteText cb "DEBUG"
  Clipboard.unsafeReadText cb >>= print

testGlobalShortcut :: IO ()
testGlobalShortcut = void $ do
  gs <- GlobalShortcut.getGlobalShortcut
  GlobalShortcut.register gs "Shift+5" undefined

testShell :: IO ()
testShell = void $ do
  sh <- Shell.unsafeGetShell
  Shell.unsafeBeep sh
  -- Shell.unsafeShowItemInFolder sh "/etc/nixos/configuration.nix"
  Shell.unsafeOpenItem sh "/etc/nixos/configuration.nix"
  Shell.unsafeOpenExternal sh "https://archive.org"
  -- FIXME: test Shell.unsafeOpenExternalDarwin
  -- FIXME: test Shell.unsafeMoveItemToTrash
  -- FIXME: test Shell.unsafeWriteShortcutLink
  -- FIXME: test Shell.unsafeReadShortcutLink

main :: IO ()
main = do
  testWindow
  -- testTray
  testClipboard
  testGlobalShortcut
  testShell
