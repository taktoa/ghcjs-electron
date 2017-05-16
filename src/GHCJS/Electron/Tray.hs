{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

-- | FIXME: doc
module GHCJS.Electron.Tray
  ( module Exported
  , module GHCJS.Electron.Tray
  ) where

import           Data.Ord
import           GHCJS.Electron.Types
import           GHCJS.Electron.Utility  as Exported
import           GHCJS.Types

import           GHCJS.Node.EventEmitter

-- | FIXME: doc
data TrayIcon
  = TrayIconImage !Image
  | TrayIconPath  !Path
  deriving (Generic)

-- | FIXME: doc
data TrayBalloonOptions
  = MkTrayBalloonOptions
    { icon    :: Maybe TrayIcon
    , title   :: Maybe JSString
    , content :: Maybe JSString
    }
  deriving (Generic)

-- | FIXME: doc
data TrayClickEvent
  = MkTrayClickEvent
    { altKey   :: Bool
    , shiftKey :: Bool
    , ctrlKey  :: Bool
    , metaKey  :: Bool
    }
  deriving (Generic)

-- | FIXME: doc
data Point
  = MkPoint
    { pointX :: Int
    , pointY :: Int
    }
  deriving (Generic)

-- | FIXME: doc
data Rectangle
  = MkRectangle
    { rectangleX      :: Int
    , rectangleY      :: Int
    , rectangleWidth  :: Int
    , rectangleHeight :: Int
    }
  deriving (Generic)

-- | FIXME: doc
data TrayEvent (platforms :: [Platform]) where
  TrayEventClick         :: TrayClickEvent -- ^ event
                         -> Rectangle      -- ^ bounds
                         -> TrayEvent '[Linux, Darwin, Win32]
  TrayEventRightClick    :: TrayClickEvent -- ^ event
                         -> Rectangle      -- ^ bounds
                         -> TrayEvent '[Darwin, Win32]
  TrayEventDoubleClick   :: TrayClickEvent -- ^ event
                         -> Rectangle      -- ^ bounds
                         -> TrayEvent '[Darwin, Win32]
  TrayEventBalloonShow   :: TrayEvent '[Win32]
  TrayEventBalloonClick  :: TrayEvent '[Win32]
  TrayEventBalloonClosed :: TrayEvent '[Win32]
  TrayEventDrop          :: TrayEvent '[Darwin]
  TrayEventDropFiles     :: Array Path -- ^ files
                         -> TrayEvent '[Darwin]
  TrayEventDropText      :: JSString -- ^ text
                         -> TrayEvent '[Darwin]
  TrayEventDropEnter     :: TrayEvent '[Darwin]
  TrayEventDropLeave     :: TrayEvent '[Darwin]
  TrayEventDropEnd       :: TrayEvent '[Darwin]

-- | FIXME: doc
data SomeTrayEvent
  = forall (platforms :: [Platform]).
    MkSomeTrayEvent (TrayEvent platforms)

-- | Initialize a new 'Tray'.
foreign import javascript safe
  "$r = new Tray($1);"
  trayNew :: Path -> IO Tray

-- | Cast a 'Tray' to an 'EventEmitter TrayEvent'.
foreign import javascript safe
  "$r = $1;"
  trayEventEmitter :: Tray -> IO (EventEmitter SomeTrayEvent)

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

-- | Sets when the tray's icon background becomes highlighted (in blue).
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
