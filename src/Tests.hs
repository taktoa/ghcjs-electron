{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.JSString

import qualified GHCJS.Electron.Accelerator        as Electron
import qualified GHCJS.Electron.BrowserWindowProxy as Electron
import qualified GHCJS.Electron.Clipboard          as Electron
import qualified GHCJS.Electron.GlobalShortcut     as Electron
import qualified GHCJS.Electron.Tray               as Electron
import qualified GHCJS.Electron.Window             as Electron

testWindow :: IO ()
testWindow = do
  bwp <- Electron.open "https://google.com"
  Electron.browserBlur bwp
  Electron.browserEval bwp "alert(\"DEBUG!\");"
  Electron.browserClose bwp

-- testTray :: IO ()
-- testTray = do
--   Electron.trayNew ""

testClipboard :: IO ()
testClipboard = do
  cb <- Electron.getClipboard "clipboard"
  Electron.clipboardWriteText cb "DEBUG"
  Electron.clipboardReadText cb >>= print

testAccelerator :: IO ()
testAccelerator = do
  gs <- Electron.getGlobalShortcut
  Electron.register gs "Shift+5" undefined

main :: IO ()
main = do
  testWindow
  testClipboard
  testAccelerator
