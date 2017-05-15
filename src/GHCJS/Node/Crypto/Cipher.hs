{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Cipher API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_cipher here>.
module GHCJS.Node.Crypto.Cipher
  ( module GHCJS.Node.Crypto.Cipher -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

import           GHCJS.Node.Buffer
import           GHCJS.Node.Stream

-- | FIXME: doc
newtype Cipher
  = MkCipher JSVal

instance IsReadStream Cipher where
  toReadStream (MkCipher val) = MkReadStream val

instance IsWriteStream Cipher where
  toWriteStream (MkCipher val) = MkWriteStream val

-- | Create a cipher object using the given algorithm, key, and initialization
--   vector, each of which is a UTF-8 encoded string.
--
--   Valid values for the @algorithm@ argument can be seen by running
--   @openssl list-cipher-algorithm@.
foreign import javascript safe
  "$r = crypto.createCipheriv($1, $2, $3);"
  unsafeCreateCipherIV :: JSString  -- ^ @algorithm@
                       -> JSString  -- ^ @key@
                       -> JSString  -- ^ @iv@
                       -> IO Cipher -- ^ Resultant cipher object.

-- | Returns any remaining enciphered contents from the given 'Cipher'.
foreign import javascript safe
  "$r = $1.final();"
  unsafeFinal :: Cipher
              -> IO Buffer

-- | Update the cipher with the given data.
foreign import javascript safe
  "$r = $1.update($2);"
  unsafeUpdate :: Cipher
               -> Buffer
               -> IO Buffer

-- | When using an authenticated encryption mode (only @GCM@ is currently
--   supported), this function sets the value used for the additional
--   authenticated data (AAD) input parameter.
foreign import javascript safe
  "$1.setAAD($2);"
  unsafeSetAAD :: Cipher
               -> Buffer
               -> IO ()

-- | When using an authenticated encryption mode (only @GCM@ is currently
--   supported), this function returns a 'Buffer' containing the authentication
--   tag that has been computed from the input data.
--
--   This function should only be called after encryption has been completed
--   using the @cipher.final()@ method (called by 'unsafeFinal' and friends).
foreign import javascript safe
  "$r = $1.getAuthTag();"
  unsafeGetAuthTag :: Cipher
                   -> IO Buffer

-- | Sets the value of the @auto_padding@ boolean attribute on a 'Cipher'.
--
--   When using block encryption algorithms, a 'Cipher' will automatically add
--   padding to the input data to the appropriate block size.
--
--   When @auto_padding@ is false, the length of the entire input data must be a
--   multiple of the cipher's block size or cipher.final() will throw an error.
--   Disabling automatic padding is useful for non-standard padding, for
--   instance using @0x0@ instead of PKCS padding.
--
--   This function should only be called before encryption has been completed
--   using the @cipher.final()@ method (called by 'unsafeFinal' and friends).
foreign import javascript safe
  "$1.setAutoPadding($2);"
  unsafeSetAutoPadding :: Cipher
                       -> Bool
                       -> IO ()
