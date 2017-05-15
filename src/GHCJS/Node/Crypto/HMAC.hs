{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS HMAC API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_hmac here>.
module GHCJS.Node.Crypto.HMAC
  ( module GHCJS.Node.Crypto.HMAC -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- FIXME: implement crypto.createHmac
-- FIXME: implement <HMAC>.digest
-- FIXME: implement <HMAC>.update
