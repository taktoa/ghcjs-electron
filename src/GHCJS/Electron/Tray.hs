{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME: doc
module GHCJS.Electron.Tray
  ( module Exported
  , module GHCJS.Electron.Tray
  ) where

import           Data.Ord
import           GHCJS.Electron.Types
import           GHCJS.Electron.Types   as Exported (EventEmitter)
import           GHCJS.Electron.Utility as Exported
import           GHCJS.Types

data Platform = PlatLinux | PlatMacOS | PlatWindows

type Linux = 'PlatLinux
type Win32 = 'PlatWindows
type Darwin = 'PlatMacOS

data KV :: Symbol -> k -> Type where
  ProxyKV :: KV key val

type (▶) k v = KV k v

data Arg :: Symbol -> k -> Type where
  ProxyArg :: Arg name ty

type (↦) k v = Arg k v

data Array ty = ProxyArray

data Optional ty = ProxyOptional

data (∪) (tyA :: Type) (tyB :: Type) = ProxyUnion

data Event (supportedPlatforms :: [Type])
           (arguments :: [Type])

type TrayBalloonOptions
  = '[ "icon"    ▶ Optional (Image ∪ JSString)
     , "title"   ▶ Optional JSString
     , "content" ▶ Optional JSString
     ]

type TrayClickEvent
  = '[ "altKey"   ▶ Bool
     , "shiftKey" ▶ Bool
     , "ctrlKey"  ▶ Bool
     , "metaKey"  ▶ Bool
     ]

type Point
  = '[ "x" ▶ Int
     , "y" ▶ Int
     ]

type Rectangle
  = '[ "x"      ▶ Int
     , "y"      ▶ Int
     , "width"  ▶ Int
     , "height" ▶ Int
     ]

type TrayEvent =
  '[ "click"          ▶ '( '[Linux, Darwin, Win32]
                         , '["event" ↦ TrayClickEvent, "bounds" ↦ Rectangle] )
   , "right-click"    ▶ '( '[Darwin, Win32]
                         , '["event" ↦ TrayClickEvent, "bounds" ↦ Rectangle] )
   , "double-click"   ▶ '( '[Darwin, Win32]
                         , '["event" ↦ TrayClickEvent, "bounds" ↦ Rectangle] )
   , "balloon-show"   ▶ '( '[Win32], '[] )
   , "balloon-click"  ▶ '( '[Win32], '[] )
   , "balloon-closed" ▶ '( '[Win32], '[] )
   , "drop"           ▶ '( '[Darwin], '[] )
   , "drop-files"     ▶ '( '[Darwin]
                         , '["event" ↦ '[], "files" ↦ Array JSString] )
   , "drop-text"      ▶ '( '[Darwin]
                         , '["event" ↦ '[], "text" ↦ JSString] )
   , "drag-enter"     ▶ '( '[Darwin], '[] )
   , "drag-leave"     ▶ '( '[Darwin], '[] )
   , "drag-end"       ▶ '( '[Darwin], '[] )
   ]

-- | Initialize a new 'Tray'.
foreign import javascript safe
  "$r = new Tray($1);"
  trayNew :: Path -> IO Tray

-- | Cast a 'Tray' to an 'EventEmitter TrayEvent'.
foreign import javascript safe
  "$r = $1;"
  trayEventEmitter :: Tray -> IO (EventEmitter TrayEvent)

-- | Destroy the given 'Tray', freeing the associated resources.
foreign import javascript safe
  "$1.destroy();"
  trayDestroy :: Tray -> IO ()

-- | Set the tray icon to the given 'Image'.
foreign import javascript safe
  "$1.setImage($2);"
  traySetImage :: Tray  -- ^ The tray to modify.
               -> Image -- ^ The image to set as the tray icon.
               -> IO ()

-- | Set the tray icon to the image file at the given 'Path'.
foreign import javascript safe
  "$1.setImage($2);"
  traySetImagePath :: Tray  -- ^ The tray to modify.
                   -> Path  -- ^ The path to an image to set as the tray icon.
                   -> IO ()

-- | Set the "pressed" image for the tray to the given 'Image'.
--   NOTE: this function only works on Mac OS
foreign import javascript safe
  "$1.setPressedImage($2);"
  traySetPressedImage :: Tray  -- ^ The tray to modify.
                      -> Image -- ^ The image to set as the "pressed" image.
                      -> IO ()

-- | Set the "title" to the given 'JSString'.
--   NOTE: this function only works on Mac OS
foreign import javascript safe
  "$1.setTitle($2);"
  traySetTitle :: Tray     -- ^ The tray to modify.
               -> JSString -- ^ The title displayed next to the tray icon.
               -> IO ()

-- | Sets when the tray’s icon background becomes highlighted (in blue).
--
--   NOTE: You can use @highlightMode@ with a 'BrowserWindow' by toggling
--   between @"never"@ and @"always"@ modes when the window visibility changes.
--
--   NOTE: this function only works on Mac OS
foreign import javascript safe
  "$1.setHighlightMode($2);"
  traySetHighlightMode :: Tray
                       -- ^ The tray to modify.
                       -> JSString
                       -- ^ The mode to set. One of the following:
                       --   * @"selection"@: Highlight the tray icon when it is
                       --     clicked and also when its context menu is open.
                       --     This is the default.
                       --   * @"always"@: Always highlight the tray icon.
                       --   * @"never"@: Never highlight the tray icon.
                       -> IO ()

-- | Create a balloon popup ala Windows XP.
--   NOTE: this function only works on Windows
foreign import javascript safe
  "$1.displayBalloon();"
  trayDisplayBalloon :: Tray     -- ^ The tray to modify.
                     -> JSString -- ^ The title displayed next to the tray icon.
                     -> IO ()
