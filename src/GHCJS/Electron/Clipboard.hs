{-# LANGUAGE JavaScriptFFI #-}

-- | FIXME: doc
module GHCJS.Electron.Clipboard
  ( module Exported
  , module GHCJS.Electron.Clipboard
  ) where

import           GHCJS.Electron.Types
import           GHCJS.Electron.Types as Exported ()
import           GHCJS.Types

-- | Get the global clipboard object.
foreign import javascript safe
  "$r = require('electron').clipboard;"
  getClipboard :: IO Clipboard

-- | Get the current clipboard as plain text.
foreign import javascript safe
  "$r = $1.readText($2);"
  clipboardReadText :: Clipboard   -- ^ The clipboard object.
                    -> JSString    -- ^ The clipboard type, e.g.: "selection".
                    -> IO JSString -- ^ The text in the clipboard.

-- | Set the clipboard to the given plain text string.
foreign import javascript safe
  "$1.writeText($3, $2);"
  clipboardWriteText :: Clipboard -- ^ The clipboard object.
                     -> JSString  -- ^ The clipboard type, e.g.: "selection".
                     -> JSString  -- ^ The contents to set the clipboard to.
                     -> IO ()

-- | Get the current clipboard as an HTML string.
foreign import javascript safe
  "$r = $1.readHTML($2);"
  clipboardReadHTML :: Clipboard   -- ^ The clipboard object.
                    -> JSString    -- ^ The clipboard type, e.g.: "selection".
                    -> IO JSString -- ^ The HTML in the clipboard.

-- | Set the clipboard to the given HTML string.
foreign import javascript safe
  "$1.writeHTML($3, $2);"
  clipboardWriteText :: Clipboard -- ^ The clipboard object.
                     -> JSString  -- ^ The clipboard type, e.g.: "selection".
                     -> JSString  -- ^ The contents to set the clipboard to.
                     -> IO ()

-- | Get the image content in the clipboard as an 'Image'/@NativeImage@.
foreign import javascript safe
  "$r = $1.readImage($2);"
  clipboardReadImage :: Clipboard -- ^ The clipboard object.
                     -> JSString  -- ^ The clipboard type, e.g.: "selection".
                     -> IO Image  -- ^ The clipboard image content.

-- | Set the image content in the clipboard to the given 'Image'/@NativeImage@.
foreign import javascript safe
  "$1.writeImage($3, $2);"
  clipboardWriteImage :: Clipboard -- ^ The clipboard object.
                      -> JSString  -- ^ The clipboard type, e.g.: "selection".
                      -> Image     -- ^ The contents to set the clipboard to.
                      -> IO ()

-- | Get the current clipboard as RTF.
foreign import javascript safe
  "$r = $1.readRTF($2);"
  clipboardReadRTF :: Clipboard   -- ^ The clipboard object.
                   -> JSString    -- ^ The clipboard type, e.g.: "selection".
                   -> IO JSString -- ^ The RTF in the clipboard.

-- | Set the clipboard to the given RTF string.
foreign import javascript safe
  "$1.writeRTF($3, $2);"
  clipboardWriteRTF :: Clipboard -- ^ The clipboard object.
                    -> JSString  -- ^ The clipboard type, e.g.: "selection".
                    -> JSString  -- ^ The contents to set the clipboard to.
                    -> IO ()

-- | Returns a 'JSVal' that is a JavaScript object with the following keys:
--   * @title@, which maps to the name of the bookmark in the clipboard.
--   * @url@, which maps to the URL of the bookmark in the clipboard.
--   This function only works on Windows and Mac OS.
foreign import javascript safe
  "$1.readBookmark();"
  clipboardReadBookmark :: Clipboard   -- ^ The clipboard object.
                        -> IO Bookmark -- ^ The resultant bookmark object.

-- | Writes a bookmark with the given title and URL into the given clipboard.
--   This function only works on Windows and Mac OS.
foreign import javascript safe
  "$1.writeBookmark($3, $4, $2);"
  clipboardWriteBookmark :: Clipboard -- ^ The clipboard object.
                         -> JSString  -- ^ The clipboard type.
                         -> JSString  -- ^ The bookmark title.
                         -> URL       -- ^ The bookmark URL.
                         -> IO ()

-- not implemented: clipboard.readFindText
-- reason:          Mac OS only and uses synchronous IPC

-- not implemented: clipboard.writeFindText
-- reason:          Mac OS only and uses synchronous IPC

-- | Clear the current clipboard state.
foreign import javascript safe
  "$1.clear($2);"
  clipboardClear :: Clipboard -- ^ The clipboard object.
                 -> JSString  -- ^ The clipboard type, e.g.: "selection".
                 -> IO ()

-- | Get the supported formats for the given clipboard.
foreign import javascript safe
  "$1.availableFormats($2);"
  clipboardFormats :: Clipboard -- ^ The clipboard object.
                   -> JSString  -- ^ The clipboard type, e.g.: "selection".
                   -> IO JSVal  -- ^ An array of supported format strings.

-- not implemented: clipboard.has
-- reason:          subsumed by availableFormats

-- not implemented: clipboard.read
-- reason:          experimental and not very useful

-- not implemented: clipboard.readBuffer
-- reason:          experimental and not very useful

-- not implemented: clipboard.write
-- reason:          not very useful
