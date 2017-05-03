{-# LANGUAGE DeriveGeneric #-}

-- FIXME: doc
module GHCJS.Array
  ( Array (..)
  ) where

import           GHCJS.Types

import           GHC.Generics

-- FIXME: doc
newtype Array t
  = MkArray JSVal
  deriving (Generic)
