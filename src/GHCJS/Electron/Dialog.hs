{-# LANGUAGE JavaScriptFFI #-}

-- FIXME: doc
module GHCJS.Electron.Dialog
  ( module Exported
  , module GHCJS.Electron.Dialog
  ) where

import           GHCJS.Electron.Types
import           GHCJS.Electron.Types as Exported (Dialog (..))
import           GHCJS.Types

-- FIXME: replace these "Options object"s with proper type definitions.

-- | Get the value of @require('electron').dialog@.
foreign import javascript safe
  "$r = require('electron').dialog;"
  getDialog :: IO Dialog

-- | Show an "open file" dialog to the user.
foreign import javascript safe
  "$1.showOpenDialog($2, $3);"
  dialogShowOpen :: Dialog
                 -> BrowserWindow     -- ^ Window on which to open the dialog.
                 -> JSVal             -- ^ Options object.
                 -> IO (JSArray Path) -- ^ An array of file paths from the user.

-- | Show an "save file" dialog to the user.
foreign import javascript safe
  "$1.showSaveDialog($2, $3);"
  dialogShowSave :: Dialog
                 -> BrowserWindow -- ^ Window on which to open the dialog.
                 -> JSVal         -- ^ Options object.
                 -> IO Path       -- ^ The file path chosen for saving.

-- | This function shows a message box. Note that it will block the process
--   until the message box is closed.
foreign import javascript safe
  "$1.showMessageBox($2, $3);"
  dialogShowMessageBox :: Dialog
                       -> BrowserWindow -- ^ Window on which to open the box.
                       -> JSVal         -- ^ Options object.
                       -> IO Int        -- ^ The index of the clicked button.

-- | Display a modal dialog box for error message reporting.
--
--   If this is called before the GUI is ready, the message may be emitted
--   @stderr@ instead, especially on Linux. To mitigate this, avoid using
--   this function before the @ready@ event when running on Linux.
foreign import javascript safe
  "$1.showErrorBox($2, $3);"
  dialogShowErrorBox :: Dialog
                     -> JSString -- ^ The title to display in the error box.
                     -> JSString -- ^ The text content of the error box.
                     -> IO ()

-- | This function displays a modal dialog that shows a message and certificate
--   information, and gives the user the option of trusting/importing the shown
--   certificate.
--
--   NOTE: this function only works on Mac OS
foreign import javascript safe
  "$1.showCertificateTrustDialog($2, $3, () => $4);"
  dialogCertDialog :: Dialog
                   -> BrowserWindow    -- ^ Window on which to open the box.
                   -> JSVal            -- ^ Options object.
                   -> Callback (IO ()) -- ^ A callback to run once done.
                   -> IO ()
