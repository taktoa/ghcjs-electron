{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Certificate API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_certificate here>.
module GHCJS.Node.Crypto.Certificate
  ( SPKAC (..), Challenge (..), PublicKey (..)
  , spkacChallenge, spkacPublicKey, spkacVerify
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

import           GHCJS.Node.Buffer

-- | A SPKAC, or Signed Public Key and Challenge, is a buffer representing
--   a certificate signing request.
newtype SPKAC
  = MkSPKAC Buffer

-- | A Challenge is a buffer representing the challenge component of a SPKAC.
newtype Challenge
  = MkChallenge Buffer

-- | A PublicKey is a buffer representing the public key component of a SPKAC.
newtype PublicKey
  = MkPublicKey Buffer

-- | Given a 'SPKAC', returns the challenge component.
foreign import javascript safe
  "$r = crypto.Certificate().exportChallenge($1);"
  spkacChallenge :: SPKAC
                 -> IO Challenge

-- | Given a 'SPKAC', returns the public key component.
foreign import javascript safe
  "$r = crypto.Certificate().exportPublicKey($1);"
  spkacPublicKey :: SPKAC
                 -> IO PublicKey

-- | Given a 'SPKAC', return @True@ if it is valid and @False@ otherwise.
foreign import javascript safe
  "$r = crypto.Certificate().verifySpkac($1);"
  spkacVerify :: SPKAC
              -> IO Bool
