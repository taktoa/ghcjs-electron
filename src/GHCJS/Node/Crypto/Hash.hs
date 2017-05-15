{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS hashing API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_hash here>.
module GHCJS.Node.Crypto.Hash
  ( module GHCJS.Node.Crypto.Hash -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- FIXME: implement crypto.createHash
-- FIXME: implement <Hash>.digest
-- FIXME: implement <Hash>.update
