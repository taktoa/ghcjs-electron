{-# LANGUAGE JavaScriptFFI #-}

-- | A wrapper over the Electron content tracing API, as documented
--   <https://electron.atom.io/docs/api/content-tracing here>.
--
--   Note: You should not use this module until the @ready@ event of the
--   app module has been emitted.
module GHCJS.Electron.ContentTracing
  ( module GHCJS.Electron.ContentTracing -- FIXME: specific export list
  ) where

import           GHCJS.Electron.Types

-- FIXME: doc
newtype ContentTracing
  = MkContentTracing JSVal

-- FIXME: doc
foreign import javascript safe
  "$r = require('electron').contentTracing;"
  unsafeGetContentTracing :: IO ContentTracing

-- | Get a set of category groups. Note that the category groups can change as
--   new code paths are reached.
--
--   Once all child processes have acknowledged the @getCategories@ request the
--   callback is invoked with an array of category group strings.
foreign import javascript safe
  "$1.getCategories($2);"
  unsafeGetCategories :: ContentTracing
                      -> Callback (Array JSString -> IO ())
                      -> IO ()

-- | Start recording on all processes.
--
--   Recording begins immediately locally and asynchronously on child processes
--   as soon as they receive the EnableRecording request. The callback will be
--   called once all child processes have acknowledged the @startRecording@
--   request.
--
--   == @categoryFilter@
--
--   The @categoryFilter@ is a glob string representing a filter on the category
--   groups that should be traced. A filter can have an optional @-@ prefix to
--   exclude category groups that contain a matching category. Having both
--   included and excluded category patterns in the same list is not supported.
--
--   Examples:
--
--   * @"test_MyTest*"@
--   * @"test_MyTest*,test_OtherStuff"@
--   * @"-excluded_category1,-excluded_category2"@
--
--   == @traceOptions@
--
--   The @traceOptions@ is a string representing the kind of tracing enabled.
--   It is a comma-delimited list where each element is one of the following:
--
--   * @record-until-full@
--   * @record-continuously@
--   * @trace-to-console@
--   * @enable-sampling@
--   * @enable-systrace@
--
--   The first three options (@record-until-full@, @record-continuously@, and
--   @trace-to-console@) represent the trace recording mode and are therefore
--   mutually exclusive. The default recording mode is @record-until-full@.
--
--   Sampling and systrace default to being disabled.
foreign import javascript safe
  "$1.startRecording({categoryFilter: $2, traceOptions: $3}, $4);"
  unsafeStartRecording :: ContentTracing
                       -> JSString
                       -- ^ @categoryFilter@
                       -> JSString
                       -- ^ @traceOptions@
                       -> Callback (IO ())
                       -> IO ()

-- | Stop recording on all processes.
--
--   Child processes typically cache trace data and only rarely flush and send
--   trace data back to the main process. This helps to minimize the runtime
--   overhead of tracing since sending trace data over IPC can be an expensive
--   operation. So, to end tracing, we must asynchronously ask all child
--   processes to flush any pending trace data.
--
--   Once all child processes have acknowledged the @stopRecording@ request, the
--   given callback will be called with the path of the file that contains the
--   traced data.
--
--   Trace data will be written into the given path if it is not @null@ or into
--   a temporary file otherwise. The actual file path will be passed to callback
--   if it's not null.
foreign import javascript safe
  "$1.stopRecording($2, $3);"
  unsafeStopRecording :: ContentTracing
                      -> Path
                      -> Callback (Path -> IO ())
                      -> IO ()

-- | Start monitoring on all processes.
--
--   Monitoring begins immediately locally and asynchronously on child processes
--   as soon as they receive the @startMonitoring@ request.
--
--   Once all child processes have acknowledged the @startMonitoring@ request
--   the given callback will be called.
--
--   The semantics of the @categoryFilter@ and @traceOptions@ parameters are
--   described in the documentation for 'unsafeStartRecording'.
foreign import javascript safe
  "$1.startMonitoring({categoryFilter: $2, traceOptions: $3}, $4);"
  unsafeStartMonitoring :: ContentTracing
                        -> JSString
                        -- ^ @categoryFilter@
                        -> JSString
                        -- ^ @traceOptions@
                        -> Callback (IO ())
                        -> IO ()

-- | Stop recording on all processes.
--
--   Once all child processes have acknowledged the @stopMonitoring@ request the
--   given callback is called.
foreign import javascript safe
  "$1.stopMonitoring($2);"
  unsafeStopMonitoring :: ContentTracing
                       -> Callback (IO ())
                       -> IO ()

-- | Get the current monitoring traced data.
--
--   Child processes typically cache trace data and only rarely flush and send
--   trace data back to the main process. This is because it may be an expensive
--   operation to send the trace data over IPC and we would like to avoid
--   unneeded runtime overhead from tracing. So, to end tracing, we must
--   asynchronously ask all child processes to flush any pending trace data.
--
--   Once all child processes have acknowledged the @captureMonitoringSnapshot@
--   request the given callback will be called with the path of the file that
--   contains the traced data.
foreign import javascript safe
  "$1.captureMonitoringSnapshot($2, $3);"
  unsafeCaptureMonitoringSnapshot :: ContentTracing
                                  -> Path
                                  -> Callback (Path -> IO ())
                                  -> IO ()
