{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Temporary library helpers for creating test audio files.
--
-- Provides utilities for creating temporary directory structures
-- with test audio files for scanner and integration tests.
module Helpers.TempLibrary
  ( -- * Temp Library Creation
    withTempLibrary
  , withTempLibraryStructure
    -- * File Creation
  , createTestAudioFile
  , createAlbumDirectory
    -- * Directory Structure Builders
  , AlbumSpec(..)
  , TrackSpec(..)
  , mkSimpleAlbum
  ) where

import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Bits ((.&.), shiftR)
import Text.Printf (printf)

-- | Specification for creating a test album.
data AlbumSpec = AlbumSpec
  { albumArtist :: Text
  , albumTitle :: Text
  , albumTracks :: [TrackSpec]
  } deriving (Show, Eq)

-- | Specification for creating a test track.
data TrackSpec = TrackSpec
  { trackNumber :: Int
  , trackTitle :: Text
  , trackArtist :: Maybe Text  -- ^ Override artist (for compilations)
  , trackDuration :: Maybe Double
  } deriving (Show, Eq)

-- | Create a simple album spec with numbered tracks.
mkSimpleAlbum :: Text -> Text -> Int -> AlbumSpec
mkSimpleAlbum artist title trackCount =
  let tracks = [ TrackSpec i ("Track " <> show i) Nothing (Just 180.0)
               | i <- [1..trackCount]
               ]
  in AlbumSpec artist title tracks

-- | Create a temporary library and run action with it.
--
-- The library is a temporary directory that is cleaned up after the action.
withTempLibrary :: Text -> (FilePath -> IO a) -> IO a
withTempLibrary name action =
  withSystemTempDirectory (toString name) action

-- | Create a temporary library with a specified structure.
--
-- Takes a list of album specifications and creates the full directory
-- structure with test audio files.
withTempLibraryStructure :: [AlbumSpec] -> (FilePath -> IO a) -> IO a
withTempLibraryStructure albums action = do
  withTempLibrary "test-library" $ \libPath -> do
    -- Create all albums
    forM_ albums $ \album -> do
      createAlbumDirectory libPath album
    action libPath

-- | Create an album directory with tracks.
--
-- Creates the directory structure: Artist/Album/tracks
createAlbumDirectory :: FilePath -> AlbumSpec -> IO ()
createAlbumDirectory basePath AlbumSpec{..} = do
  let artistDir = basePath </> toString albumArtist
      albumDir = artistDir </> toString albumTitle

  -- Create directory structure
  createDirectoryIfMissing True albumDir

  -- Create test audio files for each track
  forM_ albumTracks $ \track -> do
    let trackFile = albumDir </> formatTrackFilename track
    createTestAudioFile trackFile (albumArtist, albumTitle, track)

-- | Format a track filename.
formatTrackFilename :: TrackSpec -> FilePath
formatTrackFilename TrackSpec{..} =
  let trackNum = T.pack $ printf "%02d" trackNumber
  in toString $ trackNum <> " - " <> trackTitle <> ".flac"

-- | Create a test audio file with embedded metadata.
--
-- Creates a minimal FLAC file with the specified metadata.
-- Note: This creates a valid FLAC file header but not actual audio data.
-- The monatone library should be able to read the metadata.
createTestAudioFile :: FilePath -> (Text, Text, TrackSpec) -> IO ()
createTestAudioFile path (artist, album, TrackSpec{..}) = do
  -- Ensure parent directory exists
  createDirectoryIfMissing True (takeDirectory path)

  -- Create a minimal FLAC file with metadata
  -- For testing purposes, we create a simple file that monatone can parse
  -- This is a simplified FLAC file with just the header and metadata blocks

  -- FLAC file format:
  -- - fLaC signature (4 bytes)
  -- - Metadata blocks
  -- - Audio frames

  let flacSignature = BS.pack [0x66, 0x4C, 0x61, 0x43]  -- "fLaC"

      -- STREAMINFO metadata block (required, type 0)
      -- Format: [last_flag|type (1 byte)] [length (3 bytes)] [data]
      -- This is NOT the last block - vorbis comment comes after
      streamInfoType = 0x00  -- Not last metadata block, type 0 (STREAMINFO)
      streamInfoLength = BS.pack [0x00, 0x00, 0x22]  -- 34 bytes
      streamInfoData = BS.replicate 34 0x00  -- Minimal STREAMINFO data

      streamInfoBlock = BS.concat [BS.singleton streamInfoType, streamInfoLength, streamInfoData]

      -- Vorbis Comment metadata block (type 4) for tags
      -- This is where we'd put the actual metadata
      -- Format is complex, so for testing we'll create a minimal version
      vorbisCommentType = 0x84  -- Last block, type 4 (VORBIS_COMMENT)

      -- Create vorbis comments
      vendorString = encodeUtf8 ("reference libFLAC 1.3.0" :: Text)
      vendorLength = BS.pack $ encodeInt32LE (BS.length vendorString)

      -- Create comment fields
      titleComment = encodeUtf8 (("TITLE=" <> trackTitle) :: Text)
      artistComment = encodeUtf8 (("ARTIST=" <> fromMaybe artist trackArtist) :: Text)
      albumComment = encodeUtf8 (("ALBUM=" <> album) :: Text)
      albumArtistComment = encodeUtf8 (("ALBUMARTIST=" <> artist) :: Text)
      trackNumComment = encodeUtf8 (("TRACKNUMBER=" <> show trackNumber) :: Text)

      comments = [titleComment, artistComment, albumComment, albumArtistComment, trackNumComment]
      commentsCount = BS.pack $ encodeInt32LE (length comments)

      encodedComments = BS.concat $ map (\c ->
        let len = BS.pack $ encodeInt32LE (BS.length c)
        in BS.concat [len, c]
        ) comments

      vorbisCommentData = BS.concat [vendorLength, vendorString, commentsCount, encodedComments]
      vorbisCommentLength = BS.pack $ encodeInt24BE (BS.length vorbisCommentData)

      vorbisCommentBlock = BS.concat [BS.singleton vorbisCommentType, vorbisCommentLength, vorbisCommentData]

      -- Combine all blocks
      flacData = BS.concat [flacSignature, streamInfoBlock, vorbisCommentBlock]

  -- Write the file
  BS.writeFile path flacData

-- Helper to encode Int32 as little-endian bytes
encodeInt32LE :: Int -> [Word8]
encodeInt32LE n =
  [ fromIntegral (n .&. 0xFF)
  , fromIntegral ((n `shiftR` 8) .&. 0xFF)
  , fromIntegral ((n `shiftR` 16) .&. 0xFF)
  , fromIntegral ((n `shiftR` 24) .&. 0xFF)
  ]

-- Helper to encode Int24 as big-endian bytes (for FLAC block length)
encodeInt24BE :: Int -> [Word8]
encodeInt24BE n =
  [ fromIntegral ((n `shiftR` 16) .&. 0xFF)
  , fromIntegral ((n `shiftR` 8) .&. 0xFF)
  , fromIntegral (n .&. 0xFF)
  ]
