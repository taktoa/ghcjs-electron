{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS DiffieHellman API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_diffiehellman here>.
module GHCJS.Node.Crypto.DiffieHellman
  ( module GHCJS.Node.Crypto.DiffieHellman -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

import           GHCJS.Node.Buffer
import           GHCJS.Node.Stream

-- | FIXME: doc
newtype DiffieHellman
  = MkDiffieHellman JSVal

-- | FIXME: doc
foreign import javascript safe
  "$r = crypto.createDiffieHellman($1, $2);"
  unsafeCreateDiffieHellman :: Int    -- ^ @prime_length@
                            -> Buffer -- ^ @generator@
                            -> IO DiffieHellman

-- | FIXME: doc
foreign import javascript safe
  "$r = crypto.createDiffieHellman($1, 'buffer', $2, 'buffer');"
  unsafeCreateDiffieHellmanWithPrime :: Buffer -- ^ @prime@
                                     -> Buffer -- ^ @generator@
                                     -> IO DiffieHellman

-- FIXME: implement <DiffieHellman>.computeSecret
-- FIXME: implement <DiffieHellman>.generateKeys
-- FIXME: implement <DiffieHellman>.getGenerator
-- FIXME: implement <DiffieHellman>.getPrime
-- FIXME: implement <DiffieHellman>.getPrivateKey
-- FIXME: implement <DiffieHellman>.getPublicKey
-- FIXME: implement <DiffieHellman>.setPrivateKey
-- FIXME: implement <DiffieHellman>.setPublicKey
-- FIXME: implement <DiffieHellman>.verifyError
