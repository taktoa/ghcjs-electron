{-# LANGUAGE JavaScriptFFI #-}

-- | FIXME: doc
module GHCJS.Electron.Clipboard
  ( module Exported
  , module GHCJS.Electron.Clipboard
  ) where

import           GHCJS.Electron.Types
import           GHCJS.Electron.Types as Exported ()
import           GHCJS.Types

-- | Get the clipboard object for a given clipboard object.
foreign import javascript safe
  "$r = { type: $1, obj: require('electron').clipboard };"
  unsafeGetClipboard :: JSString
                     -- ^ A clipboard type, e.g.: @"selection"@, @"clipboard"@.
                     -> IO Clipboard
                     -- ^ A clipboard object.

-- | Get the type of a given clipboard object.
foreign import javascript safe
  "$r = $1.type;"
  unsafeClipboardType :: Clipboard
                      -- ^ A clipboard object.
                      -> IO JSString
                      -- ^ A clipboard type, e.g.: @"selection"@, @"clipboard"@.

-- | Get the current clipboard as plain text.
foreign import javascript safe
  "$r = $1.obj.readText($1.type);"
  unsafeReadText :: Clipboard   -- ^ The clipboard object.
                 -> IO JSString -- ^ The text in the clipboard.

-- | Set the clipboard to the given plain text string.
foreign import javascript safe
  "$1.obj.writeText($2, $1.type);"
  unsafeWriteText :: Clipboard -- ^ The clipboard object.
                  -> JSString  -- ^ The contents to set the clipboard to.
                  -> IO ()

-- | Get the current clipboard as an HTML string.
foreign import javascript safe
  "$r = $1.obj.readHTML($1.type);"
  unsafeReadHTML :: Clipboard -- ^ The clipboard object.
                 -> IO HTML   -- ^ The HTML in the clipboard.

-- | Set the clipboard to the given HTML string.
foreign import javascript safe
  "$1.obj.writeHTML($2, $1.type);"
  unsafeWriteHTML :: Clipboard -- ^ The clipboard object.
                  -> HTML      -- ^ The contents to set the clipboard to.
                  -> IO ()

-- | Get the image content in the clipboard as an 'Image'/@NativeImage@.
foreign import javascript safe
  "$r = $1.obj.readImage($1.type);"
  unsafeReadImage :: Clipboard -- ^ The clipboard object.
                  -> IO Image  -- ^ The clipboard image content.

-- | Set the image content in the clipboard to the given 'Image'/@NativeImage@.
foreign import javascript safe
  "$1.obj.writeImage($2, $1.type);"
  unsafeWriteImage :: Clipboard -- ^ The clipboard object.
                   -> Image     -- ^ The contents to set the clipboard to.
                   -> IO ()

-- | Get the current clipboard as RTF.
foreign import javascript safe
  "$r = $1.obj.readRTF($1.type);"
  unsafeReadRTF :: Clipboard -- ^ The clipboard object.
                -> IO RTF    -- ^ The RTF in the clipboard.

-- | Set the clipboard to the given RTF string.
foreign import javascript safe
  "$1.obj.writeRTF($2, $1.type);"
  unsafeWriteRTF :: Clipboard -- ^ The clipboard object.
                 -> RTF       -- ^ The contents to set the clipboard to.
                 -> IO ()

-- | Returns a 'JSVal' that is a JavaScript object with the following keys:
--   * @title@, which maps to the name of the bookmark in the clipboard.
--   * @url@, which maps to the URL of the bookmark in the clipboard.
--   This function only works on Windows and Mac OS.
foreign import javascript safe
  "$1.obj.readBookmark();"
  unsafeReadBookmark :: Clipboard   -- ^ The clipboard object.
                     -> IO Bookmark -- ^ The resultant bookmark object.

-- | Writes a bookmark with the given title and URL into the given clipboard.
--   This function only works on Windows and Mac OS.
foreign import javascript safe
  "$1.obj.writeBookmark($2, $3, $1.type);"
  unsafeWriteBookmark :: Clipboard -- ^ The clipboard object.
                      -> JSString  -- ^ The bookmark title.
                      -> URL       -- ^ The bookmark URL.
                      -> IO ()

-- not implemented: clipboard.readFindText
-- reason:          Mac OS only and uses synchronous IPC

-- not implemented: clipboard.writeFindText
-- reason:          Mac OS only and uses synchronous IPC

-- | Clear the current clipboard state.
foreign import javascript safe
  "$1.obj.clear($1.type);"
  unsafeClear :: Clipboard -- ^ The clipboard object.
              -> IO ()

-- | Get the supported formats for the given clipboard.
foreign import javascript safe
  "$1.obj.availableFormats($1.type);"
  unsafeAvailableFormats :: Clipboard -- ^ The clipboard object.
                         -> IO (Array JSString)
                         -- ^ An array of supported format strings.

-- not implemented: clipboard.has
-- reason:          subsumed by availableFormats

-- not implemented: clipboard.read
-- reason:          experimental and not very useful

-- not implemented: clipboard.readBuffer
-- reason:          experimental and not very useful

-- not implemented: clipboard.write
-- reason:          not very useful
