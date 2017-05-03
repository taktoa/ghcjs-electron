{-# LANGUAGE JavaScriptFFI #-}

-- | A wrapper over the Electron shell API, as documented
--   <https://electron.atom.io/docs/api/shell here>.
module GHCJS.Electron.Shell
  ( ShortcutDetails (..), ShortcutIcon (..)
  , Shell (..)
  , unsafeGetShell
  , unsafeShowItemInFolder
  , unsafeOpenItem
  , unsafeOpenExternal
  , unsafeOpenExternalDarwin
  , unsafeMoveItemToTrash
  , unsafeBeep
  , unsafeWriteShortcutLink
  , unsafeReadShortcutLink
  ) where

import           Data.Text            (Text)

import           GHCJS.Electron.Types

import           GHCJS.Nullable
import           JavaScript.Object

-- FIXME: write high-level wrappers

-- FIXME: write (de)serialization routines for ShortcutDetails

-- | The data contained within a Windows @.lnk@ shortcut file.
data ShortcutDetails
  = MkShortcutDetails
    { target         :: Text
    -- ^ The target path to launch from this shortcut.
    , cwd            :: Text
    -- ^ The working directory in which the @target@ will run.
    , args           :: Text
    -- ^ The arguments to be applied to @target@ when launching the shortcut.
    , description    :: Text
    -- ^ The description of the shortcut.
    , icon           :: ShortcutIcon
    -- ^ The icon displayed for the shortcut.
    , appUserModelId :: Text
    -- ^ The Application User Model ID.
    --   Documented <https://archive.is/gRDAt here>.
    }

-- | The icon of a Windows shortcut. This is either a path to an icon file or
--   a path to a PE file along with a resource ID.
data ShortcutIcon
  = ShortcutIconICO
    { iconPath  :: WindowsPathRef
    -- ^ The path of an ICO file.
    }
  | ShortcutIconPE
    { iconPath  :: WindowsPathRef
    -- ^ The path of a PE (Portable Executable) file; i.e.: a DLL or EXE.
    , iconIndex :: Int
    -- ^ The resource ID of the icon.
    }

-- | An Electron @shell@ object.
newtype Shell
  = MkShell JSVal

-- | Get the current global 'Shell' object as returned by
--   @require("electron").shell@.
foreign import javascript safe
  "$r = require('electron').shell;"
  unsafeGetShell :: IO Shell

-- | Show the given file in a file manager. If possible, select the file.
--
--   Returns a boolean representing whether the item was successfully shown.
foreign import javascript safe
  "$r = $1.showItemInFolder($2);"
  unsafeShowItemInFolder :: Shell -> Path -> IO Bool

-- | Open the given file in the desktop's default manner, e.g.: with @xdg-open@
--   on most Linux systems.
--
--   Returns a boolean representing whether the item was successfully opened.
foreign import javascript safe
  "$r = $1.openItem($2);"
  unsafeOpenItem :: Shell -> Path -> IO Bool

-- | Open the given URL in the desktop's default manner, e.g.: with @xdg-open@
--   on most Linux systems.
--
--   Returns a boolean representing whether the URL was successfully opened.
foreign import javascript safe
  "$r = $1.openExternal($2);"
  unsafeOpenExternal :: Shell
                     -> URL
                     -- ^ The URL to open.
                     -> IO Bool
                     -- ^ True iff the URL was successfully opened.

-- | Open the given URL in the desktop's default manner.
--   This version of the function has extra options that are specific to Mac OS,
--   and is also asynchronous.
foreign import javascript safe
  "$1.openExternal($2, {activate: $3}, $4);"
  unsafeOpenExternalDarwin :: Shell
                           -> URL
                           -- ^ The URL to open.
                           -> Bool
                           -- ^ If true, the application will be pushed to the
                           --   foreground once launched.
                           -> Callback (Nullable Error -> IO ())
                           -- ^ A callback to run once the URL is opened.
                           -> IO ()

-- | Move the file at the given path to the "trash".
--
--   Returns a boolean representing whether or not the item was successfully
--   moved to the trash.
foreign import javascript safe
  "$r = $1.moveItemToTrash($2);"
  unsafeMoveItemToTrash :: Shell -> Path -> IO Bool

-- | Play the beep sound.
foreign import javascript safe
  "$1.beep();"
  unsafeBeep :: Shell -> IO ()

-- | Creates or updates a Windows shortcut (@.lnk@ file) at the given path.
--
--   Returns a boolean representing whether or not the shortcut was created
--   successfully.
--
--   The @operation@ parameter specifies the action that will be taken and
--   should be one of the following strings:
--
--   * @"create"@
--       * Creates a new shortcut, overwriting if necessary.
--   * @"update"@
--       * Updates specified properties only on an existing shortcut.
--   * @"replace"@
--       * Overwrites an existing shortcut.
--       * This action fails hard if the shortcut doesn't exist.
--
--   NOTE: this function only works on Windows.
foreign import javascript safe
  "$r = $1.writeShortcutLink($2, $3, $4);"
  unsafeWriteShortcutLink :: Shell
                          -> Path
                          -- ^ The path to write a shortcut file to.
                          -> JSString
                          -- ^ The @operation@ to perform.
                          -> Object
                          -- ^ The (serialized) 'ShortcutDetails' to use.
                          -> IO Bool

-- | Reads and parses a Windows shortcut (@.lnk@ file) at the given path.
--
--   Returns the parsed 'ShortcutDetails' object.
--
--   If any error occurs, an exception will be thrown.
foreign import javascript safe
  "$r = $1.readShortcutLink($2);"
  unsafeReadShortcutLink :: Shell
                         -> Path
                         -- ^ The path of a shortcut file to read.
                         -> IO Object
                         -- ^ The (serialized) 'ShortcutDetails' read from
                         --   the given path.
