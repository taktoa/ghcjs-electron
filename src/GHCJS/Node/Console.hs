{-# LANGUAGE JavaScriptFFI #-}

-- | An implementation of the NodeJS Console API, as documented
--   <https://nodejs.org/api/console.html here>.
module GHCJS.Node.Console
  ( module GHCJS.Node.Console -- FIXME: specific export list
  ) where

import           GHCJS.Array
import           GHCJS.Foreign
import           GHCJS.Foreign.Callback
import           GHCJS.Types

-- | FIXME: doc
newtype Console
  = MkConsole JSVal

-- | FIXME: doc
newtype StdoutStream
  = MkStdoutStream JSVal

-- | FIXME: doc
newtype StderrStream
  = MkStderrStream JSVal

-- | FIXME: doc
defaultStderrStream :: StderrStream
defaultStderrStream = MkStderrStream jsUndefined

-- | Returns the standard global 'Console' object, i.e.: the global variable
--   named @console@ in a NodeJS execution context.
foreign import javascript safe
  "$r = console;"
  unsafeDefaultConsole :: IO Console

-- | Create a new 'Console' object using the given standard output and standard
--   error stream objects. Pass 'defaultStderrStream' into this function if you
--   want warning and error output to be send to the standard output stream.
foreign import javascript safe
  "$r = new Console($1, $2);"
  unsafeMakeConsole :: StdoutStream
                    -> StderrStream
                    -> IO Console

-- | If the given boolean is false, print the given message to the standard
--   error stream of the given console and throw an @AssertionError@.
--   If the given boolean is true, do nothing.
foreign import javascript safe
  "$1.assert($2, $3);"
  unsafeAssert :: Console
               -> Bool
               -> JSString
               -> IO ()

-- FIXME: implement <Console>.dir

-- | Print the given string to the standard output stream of the given console,
--   with a newline appended.
foreign import javascript safe
  "$1.log('%s', $2);"
  unsafeLog :: Console
            -> JSString
            -> IO ()

-- | Print the given string to the standard error stream of the given console,
--   with a newline appended.
foreign import javascript safe
  "$1.error('%s', $2);"
  unsafeError :: Console
              -> JSString
              -> IO ()

-- | Starts a timer that can be used to compute the duration of an operation.
--   Timers are identified by a unique label string. Use the same label when
--   you call 'unsafeTimeEnd' to stop the timer. Timer durations are accurate
--   to the sub-millisecond.
foreign import javascript safe
  "$1.time($2);"
  unsafeTimeStart :: Console
                  -> JSString
                  -> IO ()

-- | Stops a timer that was started with 'unsafeTimeStart'. The label string
--   should match the one given when the timer was started. Once stopped, the
--   time for the timer will be printed to the standard output stream of the
--   given console.
foreign import javascript safe
  "$1.timeEnd($2);"
  unsafeTimeEnd :: Console
                -> JSString
                -> IO ()

-- | Print the given message and stack trace to the standard error stream of
--   the given console.
foreign import javascript safe
  "$1.trace($2);"
  unsafeTrace :: Console
              -> JSString
              -> IO ()
