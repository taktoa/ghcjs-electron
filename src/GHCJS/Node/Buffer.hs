{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Buffer API, as documented
--   <https://nodejs.org/api/buffer.html here>.
module GHCJS.Node.Buffer
  ( module GHCJS.Node.Buffer -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- | FIXME: doc
newtype Buffer
  = MkBuffer JSVal

-- | Allocate a 'Buffer' of the given size in bytes. The buffer will be filled
--   with zeroes initially.
foreign import javascript safe
  "$r = Buffer.alloc($1);"
  unsafeAlloc :: Int       -- ^ The desired length of the new 'Buffer'.
              -> IO Buffer -- ^ The resulting 'Buffer'.

-- | FIXME: doc
foreign import javascript safe
  "$r = Buffer.byteLength($1, $2);"
  unsafeStringByteLength :: JSString -- ^ The input string.
                         -> JSString -- ^ The encoding to use.
                         -> IO Int   -- ^ The number of bytes in the string.
-- | FIXME: doc
foreign import javascript safe
  "$r = Buffer.byteLength($1);"
  unsafeBufferByteLength :: Buffer -- ^ The input buffer.
                         -> IO Int -- ^ The number of bytes in the buffer.
-- | FIXME: doc
foreign import javascript safe
  "$r = Buffer.compare($1, $2);"
  unsafeCompare :: Buffer   -- ^ The first buffer.
                -> Buffer   -- ^ The second buffer.
                -> IO JSVal -- ^ FIXME: doc

-- | Concatenate all of the buffers in a given array.
foreign import javascript safe
  "$r = Buffer.concat($1);"
  unsafeConcat :: Array Buffer -- ^ An array of buffers to concatenate.
               -> IO Buffer    -- ^ The concatenated result.

-- FIXME: wrap Buffer.from
-- FIXME: wrap Buffer.isBuffer
-- FIXME: wrap Buffer.isEncoding
-- FIXME: wrap Buffer.poolSize
-- FIXME: wrap <Buffer>.compare
-- FIXME: wrap <Buffer>.copy
-- FIXME: wrap <Buffer>.entries
-- FIXME: wrap <Buffer>.equals
-- FIXME: wrap <Buffer>.includes
-- FIXME: wrap <Buffer>.indexOf
-- FIXME: wrap <Buffer>.keys
-- FIXME: wrap <Buffer>.lastIndexOf
-- FIXME: wrap <Buffer>.length
-- FIXME: wrap <Buffer>.readDoubleBE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readDoubleLE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readFloatBE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readFloatLE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readInt8(offset[, noAssert])
-- FIXME: wrap <Buffer>.readInt16BE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readInt16LE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readInt32BE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readInt32LE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readIntBE(offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.readIntLE(offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.readUInt8(offset[, noAssert])
-- FIXME: wrap <Buffer>.readUInt16BE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readUInt16LE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readUInt32BE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readUInt32LE(offset[, noAssert])
-- FIXME: wrap <Buffer>.readUIntBE(offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.readUIntLE(offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.slice([start[, end]])
-- FIXME: wrap <Buffer>.swap16()
-- FIXME: wrap <Buffer>.swap32()
-- FIXME: wrap <Buffer>.swap64()
-- FIXME: wrap <Buffer>.toJSON()
-- FIXME: wrap <Buffer>.toString([encoding[, start[, end]]])
-- FIXME: wrap <Buffer>.values()
-- FIXME: wrap <Buffer>.write(string[, offset[, length]][, encoding])
-- FIXME: wrap <Buffer>.writeDoubleBE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeDoubleLE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeFloatBE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeFloatLE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeInt8(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeInt16BE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeInt16LE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeInt32BE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeInt32LE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeIntBE(value, offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.writeIntLE(value, offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.writeUInt8(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeUInt16BE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeUInt16LE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeUInt32BE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeUInt32LE(value, offset[, noAssert])
-- FIXME: wrap <Buffer>.writeUIntBE(value, offset, byteLength[, noAssert])
-- FIXME: wrap <Buffer>.writeUIntLE(value, offset, byteLength[, noAssert])

-- | Fill the given target range in the given 'Buffer' with the contents of a
--   given fill 'Buffer'. The target range is an inclusive-exclusive pair of
--   integer offsets into the input buffer. If the target range is larger than
--   the fill buffer, the fill buffer is repeated until it covers the entire
--   input buffer, and this is then cropped to fit.
--
--   The default start offset used in the bound function is @0@.
--   The default end offset used in the bound function is the input buffer size.
foreign import javascript safe
  "$1.fill($2, $3, $4);"
  unsafeFillBuffer :: Buffer -- ^ The input buffer.
                   -> Buffer -- ^ The fill buffer.
                   -> Int    -- ^ The offset to start at (inclusive).
                   -> Int    -- ^ The offset to stop at (exclusive).
                   -> IO ()

-- | Fill the given 'Buffer' with the given string, which is decoded using the
--   given encoding. If the buffer is longer than the (encoded) fill string, the
--   fill string will be repeated as many times as is necessary to cover the
--   buffer, and is then cropped to fit.
--
--   The default encoding used in the bound function is @"utf8"@.
--   The default start offset used in the bound function is @0@.
--   The default end offset used in the bound function is the input buffer size.
foreign import javascript safe
  "$1.fill($2, $4, $5, $3);"
  unsafeFillString :: Buffer   -- ^ The input buffer.
                   -> JSString -- ^ The fill string.
                   -> JSString -- ^ The fill string encoding.
                   -> Int      -- ^ The offset to start at (inclusive).
                   -> Int      -- ^ The offset to stop at (exclusive).
                   -> IO ()
