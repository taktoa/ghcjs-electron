{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.JSString

import qualified GHCJS.Electron.Accelerator        as Accelerator
import qualified GHCJS.Electron.BrowserWindowProxy as BrowserWindowProxy
import qualified GHCJS.Electron.Clipboard          as Clipboard
import qualified GHCJS.Electron.GlobalShortcut     as GlobalShortcut
import qualified GHCJS.Electron.Tray               as Tray
import qualified GHCJS.Electron.Window             as Window

testWindow :: IO ()
testWindow = do
  bwp <- Window.open "https://google.com"
  BrowserWindowProxy.browserBlur bwp
  BrowserWindowProxy.browserEval bwp "alert(\"DEBUG!\");"
  BrowserWindowProxy.browserClose bwp

-- testTray :: IO ()
-- testTray = do
--   Tray.trayNew ""

testClipboard :: IO ()
testClipboard = do
  cb <- Clipboard.unsafeGetClipboard "clipboard"
  -- Clipboard.unsafeAvailableFormats cb >>= print
  Clipboard.unsafeWriteText cb "DEBUG"
  Clipboard.unsafeReadText cb >>= print

testGlobalShortcut :: IO ()
testGlobalShortcut = do
  gs <- GlobalShortcut.getGlobalShortcut
  GlobalShortcut.register gs "Shift+5" undefined

main :: IO ()
main = do
  testWindow
  -- testTray
  testClipboard
  testGlobalShortcut
