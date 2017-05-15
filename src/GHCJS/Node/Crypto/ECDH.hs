{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS elliptic-curve Diffie-Hellman API, as
--   documented <https://nodejs.org/api/crypto.html#crypto_class_ecdh here>.
module GHCJS.Node.Crypto.ECDH
  ( module GHCJS.Node.Crypto.ECDH -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- | An Elliptic Curve Diffie-Hellman (ECDH) key exchange object.
newtype ECDH
  = MkECDH JSVal

-- | Creates an Elliptic Curve Diffie-Hellman (ECDH) key exchange object using a
--   predefined curve specified by the given string. Use 'unsafeGetCurves' to
--   obtain a list of available curve names.
--
--   On recent OpenSSL releases, @openssl ecparam -list_curves@ will also
--   display the name and description of each available elliptic curve.
foreign import javascript safe
  "$r = crypto.createECDH($1);"
  unsafeCreateECDH :: JSString -- ^ @curve_name@
                   -> IO ECDH

-- | Get a list of supported elliptic curve names.
foreign import javascript safe
  "$r = crypto.getCurves();"
  unsafeGetCurves :: IO (Array JSString)

-- FIXME: implement <ECDH>.computeSecret
-- FIXME: implement <ECDH>.generateKeys
-- FIXME: implement <ECDH>.getPrivateKey
-- FIXME: implement <ECDH>.getPublicKey
-- FIXME: implement <ECDH>.setPrivateKey
