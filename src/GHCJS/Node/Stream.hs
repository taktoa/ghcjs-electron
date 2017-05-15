{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Stream API, as documented
--   <https://nodejs.org/api/stream.html here>.
module GHCJS.Node.Stream
  ( module GHCJS.Node.Stream -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- | FIXME: doc
newtype ReadStream
  = MkReadStream JSVal

-- | FIXME: doc
newtype WriteStream
  = MkWriteStream JSVal

-- | FIXME: doc
newtype DuplexStream
  = MkDuplexStream JSVal

-- | FIXME: doc
newtype TransformStream
  = MkTransformStream
    { fromTransformStream :: DuplexStream
      -- ^ FIXME: doc
    }

-- | FIXME: doc
class IsWriteStream stream where
  -- | FIXME: doc
  toWriteStream :: stream -> WriteStream

-- | FIXME: doc
class IsReadStream stream where
  -- | FIXME: doc
  toReadStream :: stream -> ReadStream

instance IsWriteStream WriteStream where
  toWriteStream = id

instance IsWriteStream DuplexStream where
  toWriteStream (MkDuplexStream val) = MkWriteStream val

instance IsReadStream ReadStream where
  toReadStream = id

instance IsReadStream DuplexStream where
  toReadStream (MkDuplexStream val) = MkReadStream val
