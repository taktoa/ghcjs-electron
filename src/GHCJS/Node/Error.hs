{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Error API, as documented
--   <https://nodejs.org/api/errors.html here>.
module GHCJS.Node.Error
  ( module GHCJS.Node.Error -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- | FIXME: doc
newtype Error
  = MkError JSVal
