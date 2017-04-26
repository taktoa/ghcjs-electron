{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME: doc
module GHCJS.Electron.Tray
  ( module Exported
  , module GHCJS.Electron.Tray
  ) where

import           Data.Ord

import           GHC.TypeLits
import           GHCJS.Electron.Utility as Exported
-- import           GHCJS.Electron.Types
-- import           GHCJS.Electron.Types as Exported ()
-- import           GHCJS.Types

--------------------------------------------------------------------------------

-- Type-level insertion sort for 'Symbols'; this code is partially from
-- https://kseo.github.io/posts/2017-01-30-type-level-insertion-sort.html

-- type family InsertSym x xs where
--   InsertSym x '[]       = x ': '[]
--   InsertSym x (y ': ys) = InsertSym' (CmpSymbol x y) x y ys
--
-- type family InsertSym' b x y ys where
--   InsertSym' 'LT x y ys = x ': (y ': ys)
--   InsertSym' _   x y ys = y ': InsertSym x ys
--
-- type family SortSym xs where
--   SortSym '[]       = '[]
--   SortSym (x ': xs) = InsertSym x (SortSym xs)

-- type family IsEqual (ord :: Ordering) :: Bool where
--   IsEqual 'EQ = 'True
--   IsEqual _   = 'False
--
-- type family (:≡:) (symA :: Symbol) (symB :: Symbol) :: Bool where
--   symA :≡: symB = IsEqual (CmpSymbol symA symB)

type TKey = Symbol
type TKV k = (TKey, k)

type family (:≡:) (symA :: Symbol) (symB :: Symbol) :: Bool where
  sym :≡: sym = 'True
  _   :≡:   _ = 'False

type family If (bool :: Bool) (valA :: k) (valB :: k) :: k where
  If 'True  valA _    = valA
  If 'False _    valB = valB

type family InsertPair' (b :: Ordering) (x :: TKV k) (y :: TKV k) (ys :: [TKV k]) where
  InsertPair' 'LT x y ys = x ': (y ': ys)
  InsertPair' _   x y ys = y ': InsertPair x ys

type family InsertPair (x :: TKV k) (xs :: [TKV k]) where
  InsertPair x '[] = x ': '[]
  InsertPair '(x, xV) ('(y, yV) ': ys)
    = InsertPair' (CmpSymbol x y) '(x, xV) '(y, yV) ys

type family SortPairs (xs :: [TKV k]) :: [TKV k] where
  SortPairs '[]       = '[]
  SortPairs (x ': xs) = InsertPair x (SortPairs xs)

type family UniqPairs (list :: [TKV k]) :: [TKV k] where
  UniqPairs ('(a, aVal) ': ('(b, bVal) ': rest))
    = If (a :≡: b)
         (UniqPairs ('(a, aVal) ': rest))
         ('(a, aVal) ': (UniqPairs ('(b, bVal) ': rest)))
  UniqPairs other = other

type family DedupePairs (list :: [TKV k]) :: [TKV k] where
  DedupePairs list = UniqPairs (SortPairs list)

type family Update (key :: Symbol) (val :: k) (map :: [TKV k]) where
  Update key new '[]                   = '[]
  Update key new ('(key, old) ': rest) = '(key, new) ': (Update key new rest)
  Update key new (pair        ': rest) = pair        ': (Update key new rest)

type family RBM' (mapX :: [TKV k]) (mapY :: [TKV k]) :: [TKV k] where
  RBM' mapX ('(key, val) ': rest) = RBM' (Update key val mapX) rest

type family RBM (mapX :: [TKV k]) (mapY :: [TKV k]) :: [TKV k] where
  RBM mapX mapY = DedupePairs (RBM' (DedupePairs mapX) (DedupePairs mapY))

type mapX // mapY = RBM mapX mapY

--------------------------------------------------------------------------------

data JSString
data Image

data Platform
  = Linux | MacOS | Windows

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
  '[ "click"          ▶ '( '[Linux, MacOS, Windows]
                         , '["event" ↦ TrayClickEvent, "bounds" ↦ Rectangle] )
   , "right-click"    ▶ '( '[MacOS, Windows]
                         , '["event" ↦ TrayClickEvent, "bounds" ↦ Rectangle] )
   , "double-click"   ▶ '( '[MacOS, Windows]
                         , '["event" ↦ TrayClickEvent, "bounds" ↦ Rectangle] )
   , "balloon-show"   ▶ '( '[Windows], '[] )
   , "balloon-click"  ▶ '( '[Windows], '[] )
   , "balloon-closed" ▶ '( '[Windows], '[] )
   , "drop"           ▶ '( '[MacOS], '[] )
   , "drop-files"     ▶ '( '[MacOS]
                         , '["event" ↦ '[], "files" ↦ Array JSString] )
   , "drop-text"      ▶ '( '[MacOS]
                         , '["event" ↦ '[], "text" ↦ JSString] )
   , "drag-enter"     ▶ '( '[MacOS], '[] )
   , "drag-leave"     ▶ '( '[MacOS], '[] )
   , "drag-end"       ▶ '( '[MacOS], '[] )
   ]

{-
-- | FIXME: doc
foreign import javascript safe
  "$r = new Tray($1);"
  trayNew :: Path -> IO Tray

-- | FIXME: doc
foreign import javascript safe
  "$r = $1;"
  trayEventEmitter :: Tray -> IO (EventEmitter TrayEvent)

-- | FIXME: doc
foreign import javascript safe
  "$1.destroy;"
  trayDestroy :: Tray -> IO ()

-- | FIXME: doc
foreign import javascript safe
  "$1.setImage($2);"
  traySetImage :: Tray  -- ^ The tray to modify.
               -> Image -- ^ The image to set as the tray icon.
               -> IO ()

-- | FIXME: doc
foreign import javascript safe
  "$1.setImage($2);"
  traySetImagePath :: Tray  -- ^ The tray to modify.
                   -> Path  -- ^ The path to an image to set as the tray icon.
                   -> IO ()

-- | FIXME: doc
--   NOTE: this function only works on Mac OS
foreign import javascript safe
  "$1.setPressedImage($2);"
  traySetPressedImage :: Tray  -- ^ The tray to modify.
                      -> Image -- ^ The image to set as the "pressed" image.
                      -> IO ()

-- | FIXME: doc
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

-- | FIXME: doc
--   NOTE: this function only works on Windows
foreign import javascript safe
  "$1.displayBalloon();"
  trayDisplayBalloon :: Tray     -- ^ The tray to modify.
                     -> JSString -- ^ The title displayed next to the tray icon.
                     -> IO ()
-}
