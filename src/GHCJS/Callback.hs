{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | FIXME: doc
module GHCJS.Callback
  ( module GHCJS.Callback -- FIXME: specific import list
  , module Exported
  ) where

import           Control.Monad          (join)

import           GHCJS.Foreign.Callback as Exported
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types            (IsJSVal, JSVal)
import           Unsafe.Coerce

class (~~>) x y | y -> x

instance (IO ()) ~~> (IO ())

instance (PFromJSVal a, b ~~> b') => (JSVal -> b) ~~> (a -> b')

class CallbackCoerce x y
instance (x ~~> y) => CallbackCoerce x y

coerceCallback :: (x ~~> y) => Callback x -> Callback y
coerceCallback = unsafeCoerce

makeAsyncCallback0 :: IO () -> IO (Callback (IO ()))
makeAsyncCallback0 m = fmap coerceCallback (asyncCallback m)

makeAsyncCallback1 :: (PFromJSVal a)
                   => (a -> IO ()) -> IO (Callback (a -> IO ()))
makeAsyncCallback1 f = let f' x = f (pFromJSVal x)
                       in fmap coerceCallback (asyncCallback1 f')

makeAsyncCallback2 :: (PFromJSVal a, PFromJSVal b)
                   => (a -> b -> IO ()) -> IO (Callback (a -> b -> IO ()))
makeAsyncCallback2 f = let f' x y = f (pFromJSVal x) (pFromJSVal y)
                       in fmap coerceCallback (asyncCallback2 f')

makeAsyncCallback3 :: (PFromJSVal a, PFromJSVal b, PFromJSVal c)
                   => (a -> b -> c -> IO ())
                   -> IO (Callback (a -> b -> c -> IO ()))
makeAsyncCallback3 f = let f' x y z
                             = f (pFromJSVal x) (pFromJSVal y) (pFromJSVal z)
                       in fmap coerceCallback (asyncCallback3 f')
