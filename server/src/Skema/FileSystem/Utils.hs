{-# LANGUAGE ScopedTypeVariables #-}

-- | Filesystem utilities for cross-platform file operations.
module Skema.FileSystem.Utils
  ( moveFile
  , osPathToString
  , stringToOsPath
  ) where

import qualified System.Directory as Dir
import qualified System.OsPath as OP
import System.IO.Error (IOError, ioeGetErrorType)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Encoding (mkTextEncoding)
import Control.Exception (try, throwIO)

-- | Move a file, handling cross-device links by falling back to copy+delete.
--
-- This function first attempts to use 'renameFile' (which is fast and atomic).
-- If that fails with an 'UnsupportedOperation' error (indicating a cross-device link),
-- it falls back to copying the file and then deleting the original.
--
-- This is necessary when moving files between different filesystems, such as:
-- - Local storage to network share (NFS/SMB)
-- - Different mount points
-- - Different physical devices
--
-- Note: The copy+delete fallback is NOT atomic, so if the process is interrupted
-- between copy and delete, you may end up with two copies of the file.
moveFile :: FilePath -> FilePath -> IO ()
moveFile source target = do
  result <- try $ Dir.renameFile source target
  case result of
    Right () -> pure ()
    Left (e :: IOError) -> do
      -- Check if this is a cross-device link error
      if ioeGetErrorType e == UnsupportedOperation
        then do
          -- Fall back to copy + delete for cross-device moves
          Dir.copyFile source target
          Dir.removeFile source
        else throwIO e

-- | Convert OsPath to String using UTF-8 with ROUNDTRIP mode.
--
-- Uses PEP 383 surrogateescape to preserve exact byte sequences,
-- even for invalid UTF-8. This ensures we can always reconstruct the original path.
--
-- This is the same approach used in the database repository layer.
osPathToString :: OP.OsPath -> IO String
osPathToString path = do
  enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
  case OP.decodeWith enc enc path of
    Left err -> throwIO err
    Right str -> pure str

-- | Convert String to OsPath using UTF-8 with ROUNDTRIP mode.
--
-- Uses PEP 383 surrogateescape to preserve exact byte sequences,
-- even for invalid UTF-8. This ensures we can always reconstruct the original path.
--
-- This is the same approach used in the database repository layer.
stringToOsPath :: String -> IO OP.OsPath
stringToOsPath str = do
  enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
  case OP.encodeWith enc enc str of
    Left err -> throwIO err
    Right path -> pure path
