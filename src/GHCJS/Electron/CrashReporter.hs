{-# LANGUAGE JavaScriptFFI #-}

-- | A wrapper over the Electron crash reporter API, as documented
--   <https://electron.atom.io/docs/api/crash-reporter here>.
--
--   After 'start' is called a crash reporter process will be spawned such that
--   if a crash occurs, a POST request with mimetype @multipart/form-data@ will
--   be sent to the @submitURL@. It has the following fields:
--
--   * @ver@: a string representing the version of Electron running
--   * @platform@: a string representing the current platform, e.g.: @"win32"@.
--   * @process_type@: either @"main"@ or @"renderer"@.
--   * @guid@: a string that is a globally unique identifier for this system.
--   * @_version@: the version specified in @package.json@ as a string.
--   * @_productName@: the product name specified in the 'CrashReporterOptions'.
--   * @prod@: the name of the underlying product (should be @"Electron"@).
--   * @_companyName@: the company name specified in the 'CrashReporterOptions'.
--   * @upload_file_minidump@: a Windows minidump file.
--
--   If the @_extraData@ field of the 'CrashReporterOptions' object given to
--   the 'start' function had any fields, each key-value pair @key: value@
--   will be added to the POST request as a field named @key@ with contents
--   equal to the serialization of @value@.
module GHCJS.Electron.CrashReporter where

import           Data.Text            (Text)
import qualified Data.Text            as Text

import           GHCJS.Electron.Types

import           JavaScript.Object

-- | FIXME: doc
newtype CrashReporter
  = MkCrashReporter JSVal

-- | FIXME: doc
data CrashReport
  = MkCrashReport
    { _date :: Text
    , _ID   :: Int
    }

-- | FIXME: doc
data CrashReporterOptions
  = MkCrashReporterOptions
    { _companyName         :: Maybe Text
      -- ^ The company name to provide with the crash report.
    , _submitURL           :: Text
      -- ^ Crash reports will be sent as POST requests to this URL.
    , _productName         :: Maybe Text
      -- ^ The product name. If not provided, defaults to @app.getName()@.
    , _uploadToServer      :: Bool
      -- ^ Whether crash reports should be sent to the server.
    , _ignoreSystemHandler :: Bool
      -- ^ Should the system crash handler be ignored?
    , _extraData           :: Object
      -- ^ An arbitrary JSON object to send with the crash report.
    }

-- | FIXME: doc
start :: CrashReporterOptions -> IO CrashReporter
start = undefined -- FIXME: implement

-- | Get the canonical 'CrashReporter' object, i.e.: the value of
--   @require('electron').crashReporter@.
foreign import javascript safe
  "$r = require('electron').crashReporter;"
  getCrashReporter :: IO CrashReporter
                   -- ^ A crash reporter object.

-- | Start the crash reporter with the given options.
foreign import javascript safe
  "$1.start($2);"
  unsafeStart :: CrashReporter
              -- ^ The crash reporter object to use.
              -> JSVal
              -- ^ An options object.
              -> IO ()

-- | Returns the latest crash report. If no crash reports have been sent or the
--   crash reporter has not been started, @null@ is returned instead.
foreign import javascript safe
  "$r = $1.getLastCrashReport();"
  unsafeGetLastCrashReport :: CrashReporter
                           -- ^ The crash reporter object to use.
                           -> IO JSVal -- Maybe CrashReport
                           -- ^ The last crash report if there is one.

-- | Returns a list of all uploaded crash reports to date.
foreign import javascript safe
  "$r = $1.getUploadedReports();"
  unsafeGetUploadedReports :: CrashReporter
                           -- ^ The crash reporter object to use.
                           -> IO (Array CrashReport)
                           -- ^ All uploaded crash reports.

-- not implemented: crashReporter.getUploadToServer
-- reason:          Linux/Mac OS only and unnecessary

-- not implemented: crashReporter.setUploadToServer
-- reason:          Linux/Mac OS only and unnecessary

-- not implemented: crashReporter.setExtraParameter
-- reason:          Mac OS only and probably unnecessary
