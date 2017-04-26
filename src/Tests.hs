{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.JSString
import qualified GHCJS.Electron.Accelerator        as Electron
import qualified GHCJS.Electron.BrowserWindowProxy as Electron
import qualified GHCJS.Electron.Window             as Electron

testWindow :: IO ()
testWindow = do
  bwp <- Electron.open "https://google.com"
  Electron.browserBlur bwp
  Electron.browserEval bwp "alert(\"DEBUG!\");"
  Electron.browserClose bwp

testAccelerator :: IO ()
testAccelerator = do
  gs <- Electron.getGlobalShortcut
  Electron.acceleratorRegister gs "Shift+5" undefined

main :: IO ()
main = do
  testWindow
  testAccelerator
