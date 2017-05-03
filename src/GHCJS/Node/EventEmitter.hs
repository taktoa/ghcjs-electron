{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE JavaScriptFFI          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | An implementation of the NodeJS EventEmitter API, as documented
--   <https://nodejs.org/api/events.html here>.
module GHCJS.Node.EventEmitter
  ( module GHCJS.Node.EventEmitter -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign.Callback
import           GHCJS.Types

import           JavaScript.JSON.Types  (Value)

-- FIXME: maybe move this stuff to GHCJS.Electron.Event?

-- | FIXME: doc
newtype Event
  = MkEvent JSVal

-- | FIXME: doc
foreign import javascript safe
  "$1.returnValue = $2;"
  unsafeSetReturnValue :: Event
                       -> Value
                       -> IO ()

-- | FIXME: doc
foreign import javascript safe
  "$r = $1.sender;"
  unsafeGetSender :: Event
                  -> IO JSVal

-- | FIXME: doc
foreign import javascript safe
  "(function(obj) { obj.send.apply(obj, [$2] + $3); })($1.sender);"
  unsafeReply :: Event
              -> JSString -- should be GHCJS.Electron.Types.Channel instead
              -> Array Value
              -> IO ()

-- | The name of an IPC channel.
newtype Channel
  = MkChannel JSString

-- | An instance of the NodeJS EventEmitter class.
newtype EventEmitter event
  = MkEventEmitter JSVal

-- | A typeclass representing types that can be converted to an 'EventEmitter'
--   for a given event type.
class IsEventEmitter event emitter | emitter -> event where
  -- | Convert the given value to an 'EventEmitter'.
  toEventEmitter :: emitter -> IO (EventEmitter event)

-- FIXME: functions not yet implemented:
--   - @removeListener(event: string, listener: Function): App;@
--   - @removeAllListeners(event?: string): App;@
--   - @listeners(event: string): Function[];@
--   - @emit(event: string, ...args: any[]): boolean;@
--   - @listenerCount(type: string): number;@

-- | Listen for the given event and run the given callback whenever it occurs.
foreign import javascript safe
  "$1.on($2, $3);"
  unsafeListenerOn :: EventEmitter event
                   -> JSString
                   -> Callback (JSString -> IO ())
                   -> IO ()

-- | Listen for the given event and run the given callback exactly once;
--   i.e.: the callback will be run precisely the first time the event occurs
--   after this function is run.
foreign import javascript safe
  "$1.once($2, (function() { return $3(arguments); }));"
  unsafeListenerOnce :: EventEmitter event
                     -> JSString
                     -> Callback (Array JSVal -> IO ())
                     -> IO ()

-- | Remove all listeners on the given 'EventEmitter'.
foreign import javascript safe
  "$1.removeAllListeners();"
  unsafeRemoveAllListeners :: EventEmitter event -> IO ()

-- | Remove all listeners for the given event on the given 'EventEmitter'.
foreign import javascript safe
  "$1.removeAllListeners($2);"
  unsafeRemoveAllListenersOnEvent :: EventEmitter event -> JSString -> IO ()

-- | Set the maximum number of listeners for events on the given 'EventEmitter'
--   to the given natural number.
foreign import javascript safe
  "$1.setMaxListeners($2);"
  unsafeSetMaxListeners :: EventEmitter event -> Int -> IO ()

-- | Get the maximum number of listeners for events on the given 'EventEmitter',
--   as set by 'appSetMaxListeners'.
foreign import javascript safe
  "$r = $1.getMaxListeners();"
  unsafeGetMaxListeners :: EventEmitter event -> IO Int

-- | Get the number of listeners for an event on the given 'EventEmitter'.
foreign import javascript safe
  "$r = $1.listenerCount($2);"
  unsafeListenerCount :: EventEmitter event -> JSString -> IO Int
