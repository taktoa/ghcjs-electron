{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.JSString
import qualified GHCJS.Electron.Window as Electron

testWindow :: IO ()
testWindow = do
  Electron.open "https://google.com"
  Electron.open "https://google.com"

main :: IO ()
main = do
  pure ()
