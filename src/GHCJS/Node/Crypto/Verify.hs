{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS signature verification API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_verify here>.
module GHCJS.Node.Crypto.Verify
  ( module GHCJS.Node.Crypto.Verify -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- FIXME: implement crypto.createVerify
-- FIXME: implement <Verify>.verify
-- FIXME: implement <Verify>.update
