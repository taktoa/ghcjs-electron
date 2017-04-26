{-# LANGUAGE JavaScriptFFI #-}

-- | FIXME: doc
module GHCJS.Electron.App
  ( module Exported
  , module GHCJS.Electron.App
  ) where

import           GHCJS.Electron.Types
import           GHCJS.Electron.Types as Exported (App (..))
import           GHCJS.Types

-- FIXME: functions not yet implemented:
--   - @removeListener(event: string, listener: Function): App;@
--   - @removeAllListeners(event?: string): App;@
--   - @listeners(event: string): Function[];@
--   - @emit(event: string, ...args: any[]): boolean;@
--   - @listenerCount(type: string): number;@

-- | Get the current 'App' as returned by @require("electron").app@.
foreign import javascript safe
  "$r = require('electron').app;"
  getApp :: IO App

-- | Listen for the given event and run the given callback whenever it occurs.
foreign import javascript safe
  "$1.on($2, $3);"
  appListenerOn :: App
                -> JSString
                -> Callback (JSString -> Any)
                -> IO ()

-- | Listen for the given event and run the given callback exactly once;
--   i.e.: the callback will be run precisely the first time the event occurs
--   after this function is run.
foreign import javascript safe
  "$1.once($2, $3);"
  appListenerOnce :: App
                  -> JSString
                  -> Callback (JSString -> Any)
                  -> IO ()

-- | Remove all listeners on the given 'App'.
foreign import javascript safe
  "$1.removeAllListeners();"
  appRemoveAllListeners :: App -> IO ()

-- | Remove all listeners for the given event on the given 'App'.
foreign import javascript safe
  "$1.removeAllListeners($2);"
  appRemoveAllListenersOnEvent :: App -> JSString -> IO ()

-- | Set the maximum number of listeners for events on the given 'App' to the
--   given natural number.
foreign import javascript safe
  "$1.setMaxListeners($2);"
  appSetMaxListeners :: App -> Int -> IO ()

-- | Get the maximum number of listeners for events on the given 'App', as set
--   by 'appSetMaxListeners'.
foreign import javascript safe
  "$r = $1.getMaxListeners();"
  appGetMaxListeners :: App -> IO Int

-- | Get the number of listeners for the given event on the given 'App'.
foreign import javascript safe
  "$r = $1.listenerCount($2);"
  appListenerCount :: App -> JSString -> IO Int

-- | Try to close all windows. The before-quit event will first be emitted.
--   If all windows are successfully closed, the will-quit event will be emitted
--   and by default the application would be terminated.
--
--   This method guarantees all beforeunload and unload handlers are correctly
--   executed. It is possible that a window cancels the quitting by returning
--   false in beforeunload handler.
foreign import javascript safe
  "$1.quit();"
  appQuit :: App -> IO ()

-- | Quit the application directly; it will not try to close all windows so
--   cleanup code will not run.
foreign import javascript safe
  "$1.terminate();"
  appTerminate :: App -> IO ()

-- | Returns the current application directory.
foreign import javascript safe
  "$1.getAppPath();"
  appGetAppPath :: App -> IO Path

-- | Get a special path with the given name.
foreign import javascript safe
  "$r = $1.getPath($2);"
  appGetPath :: App
             -> JSString
             -- ^ Should be one of the following:
             --   @"home"@, @"appData"@, @"userData"@, @"cache"@, @"userCache"@,
             --   @"temp"@, @"userDesktop"@, @"exe"@, @"module"@
             -> IO Path
             -- ^ The path associated with the given name.

-- | Overrides the path to a special directory or file associated with name.
--   If the path specifies a directory that does not exist, the directory will
--   be created by this method. On failure an Error would throw.
--
--   By default web pages' cookies and caches will be stored under userData
--   directory, if you want to change this location, you have to override the
--   userData path before the ready event of app module gets emitted.
foreign import javascript safe
  "$1.setPath($2, $3);"
  appSetPath :: App
             -> JSString
             -- ^ Should be one of the following:
             --   @"home"@, @"appData"@, @"userData"@, @"cache"@, @"userCache"@,
             --   @"temp"@, @"userDesktop"@, @"exe"@, @"module"@
             -> Path
             -- ^ A file path to set; will be created if it does not exist.
             -> IO ()

-- | Returns the version of loaded application.
--   If no version is found in the application's @package.json@, then the
--   version of current bundle or executable is returned instead.
foreign import javascript safe
  "$r = $1.getVersion();"
  appGetVersion :: App -> IO JSString

-- | Returns the name of the loaded application.
--   Usually the @name@ field of @package.json@ is a short lowercase string,
--   according to the NPM module specification, so usually you should also
--   specify the @productName@ field, which is your application's full
--   capitalized name, and it will be preferred over @name@ by Electron.
foreign import javascript safe
  "$r = $1.getName();"
  appGetName :: App -> IO JSString

-- | Given a URL, calls the given callback with the proxy used for that URL.
foreign import javascript safe
  "$1.resolveProxy($2, $3);"
  appResolveProxy :: App
                  -> URL
                  -- ^ The URL to resolve.
                  -> Callback (Proxy -> IO ())
                  -- ^ The callback to call once the URL is resolved.
                  -> IO ()

-- | Adds the given path to the recent documents list.
--   This list is managed by the system; on Windows you can visit the list from
--   the taskbar; on Mac OS you can visit it from the dock menu.
foreign import javascript safe
  "$1.addRecentDocument($2);"
  appAddRecent :: App
               -> Path
               -- ^ A path to add to the recent documents list.
               -> IO ()

-- | Clear the recent documents list.
foreign import javascript safe
  "$1.clearRecentDocuments();"
  appClearRecent :: App -> IO ()

-- | Adds tasks to the Tasks category of JumpList on Windows.
--   This API is /only/ available on Windows.
foreign import javascript safe
  "$1.setUserTasks($2);"
  appSetUserTasks :: App
                  -> JSVal
                  -- ^ A list of tasks to which the user tasks will be set.
                  -> IO ()

-- | FIXME: doc
foreign import javascript safe
  "$r = $1.dock;"
  appGetDock :: App -> IO BrowserWindow

-- | FIXME: doc
--   FIXME: may not make sense.
foreign import javascript safe
  "$1.dock = $2;"
  appSetDock :: App -> BrowserWindow -> IO ()

-- | FIXME: doc
foreign import javascript safe
  "$r = $1.commandLine;"
  appGetCommandLine :: App -> IO CommandLine

-- | FIXME: doc
--   FIXME: may not make sense.
foreign import javascript safe
  "$1.commandLine = $2;"
  appSetCommandLine :: App -> CommandLine -> IO ()

-- | This method makes your application a "Single Instance Application" instead
--   of allowing multiple instances of your app to run.
--   In other words, this will ensure that at most one instance of your
--   application is running at any time, and launching a new instance will
--   simply signal this instance and exit.
--   NOTE: callback type is @(args: string[], workingDirectory: string) => bool@
--   FIXME: what does the callback do? what does the returned boolean mean?
foreign import javascript safe
  "$r = $1.makeSingleInstance($2);"
  appMakeSingleInstance :: App
                        -> Callback (JSArray JSString -> JSString -> IO Bool)
                        -- ^ FIXME: doc
                        -> IO Bool

-- | FIXME: doc
foreign import javascript safe
  "$1.setAppUserModelId($2);"
  appSetUserModelId :: App
                    -> JSString
                    -- ^ FIXME: doc
                    -> IO ()
