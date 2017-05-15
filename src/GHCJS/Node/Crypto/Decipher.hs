{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Decipher API, as documented
--   <https://nodejs.org/api/crypto.html#crypto_class_decipher here>.
module GHCJS.Node.Crypto.Decipher
  ( module GHCJS.Node.Crypto.Decipher -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

import           GHCJS.Node.Buffer
import           GHCJS.Node.Stream

-- FIXME: figure out a reasonable way to avoid the duplication between this
-- module and GHCJS.Node.Crypto.Cipher

-- | FIXME: doc
newtype Decipher
  = MkDecipher JSVal

instance IsReadStream Decipher where
  toReadStream (MkDecipher val) = MkReadStream val

instance IsWriteStream Decipher where
  toWriteStream (MkDecipher val) = MkWriteStream val

-- | Create a 'Decipher' object using the given algorithm, key, and IV
--   (initialization vector), each of which is a UTF-8 encoded string.
--
--   Valid values for the @algorithm@ argument can be seen by running
--   @openssl list-cipher-algorithm@.
foreign import javascript safe
  "$r = crypto.createDecipheriv($1, $2, $3);"
  unsafeCreateDecipherIV :: JSString    -- ^ @algorithm@
                         -> JSString    -- ^ @key@
                         -> JSString    -- ^ @iv@
                         -> IO Decipher -- ^ Resultant 'Decipher' object.

-- | Returns any remaining deciphered contents from the given 'Decipher'.
foreign import javascript safe
  "$r = $1.final();"
  unsafeFinal :: Decipher
              -> IO Buffer

-- | Update the 'Decipher' with the given data.
foreign import javascript safe
  "$r = $1.update($2);"
  unsafeUpdate :: Decipher
               -> Buffer
               -> IO Buffer

-- | When using an authenticated encryption mode (only @GCM@ is currently
--   supported), this function sets the value used for the additional
--   authenticated data (AAD) input parameter.
foreign import javascript safe
  "$1.setAAD($2);"
  unsafeSetAAD :: Decipher
               -> Buffer
               -> IO ()

-- | When using an authenticated encryption mode (only @GCM@ is currently
--   supported), this function returns a 'Buffer' containing the authentication
--   tag that has been computed from the input data.
--
--   This function should only be called after encryption has been completed
--   using the @decipher.final()@ method (called by 'unsafeFinal' and friends).
foreign import javascript safe
  "$r = $1.getAuthTag();"
  unsafeGetAuthTag :: Decipher
                   -> IO Buffer

-- | Sets the value of the @auto_padding@ boolean attribute on a 'Decipher'.
--
--   When using block encryption algorithms, a 'Decipher' will automatically add
--   padding to the input data to the appropriate block size.
--
--   When @auto_padding@ is false, the length of the entire input data must be a
--   multiple of the cipher block size or decipher.final() will throw an error.
--   Disabling automatic padding is useful for non-standard padding, for
--   instance using @0x0@ instead of PKCS padding.
--
--   This function should only be called before encryption has been completed
--   using the @decipher.final()@ method (called by 'unsafeFinal' and friends).
foreign import javascript safe
  "$1.setAutoPadding($2);"
  unsafeSetAutoPadding :: Decipher
                       -> Bool
                       -> IO ()
