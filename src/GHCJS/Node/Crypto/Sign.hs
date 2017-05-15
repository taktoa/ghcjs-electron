{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS signing API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_sign here>.
module GHCJS.Node.Crypto.Sign
  ( module GHCJS.Node.Crypto.Sign -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- FIXME: implement crypto.createSign
-- FIXME: implement <Sign>.sign
-- FIXME: implement <Sign>.update
