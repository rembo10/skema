{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Typed error system for the application.
--
-- This module defines all domain errors as typed ADTs rather than using
-- String/Text or partial functions. This allows better error handling,
-- testing, and recovery strategies.
module Skema.Domain.Errors
  ( -- * Error Types
    AppError(..)
  , DatabaseError(..)
  , ServiceError(..)
  , ConfigError(..)
  , MusicBrainzError(..)
    -- * Error Handling
  , prettyError
  , logError
    -- * Combinators
  , mapLeft
  , eitherToMaybe
  , maybeToEither
  ) where

import GHC.Generics ()
import Katip

-- | Top-level application errors.
data AppError
  = DatabaseErr DatabaseError
  | ServiceErr ServiceError
  | ConfigErr ConfigError
  | MusicBrainzErr MusicBrainzError
  | UnexpectedError Text  -- For truly unexpected errors
  deriving (Show, Eq, Generic)

-- | Database operation errors.
data DatabaseError
  = TrackNotFound Int64
  | ClusterNotFound Int64
  | DownloadNotFound Int64
  | ArtistNotFound Int64
  | AlbumNotFound Int64
  | DuplicateKey Text
  | QueryFailed Text      -- SQL query failed
  | ConnectionFailed Text -- Connection pool exhausted or DB unavailable
  | MigrationFailed Text  -- Schema migration failed
  | TransactionFailed Text
  deriving (Show, Eq, Generic)

-- | Service-level errors.
data ServiceError
  = ScanFailed Text         -- File system scan failed
  | GroupingFailed Text     -- Failed to group files
  | IdentificationFailed Text
  | MetadataWriteFailed Text FilePath
  | DownloadFailed Text
  | ImportFailed Text
  | ServiceCrashed Text     -- Service died unexpectedly
  deriving (Show, Eq, Generic)

-- | Configuration errors.
data ConfigError
  = ConfigNotFound FilePath
  | ConfigParseFailed FilePath Text
  | ConfigValidationFailed Text
  | InvalidLibraryPath Text
  | InvalidDatabaseConfig Text
  deriving (Show, Eq, Generic)

-- | MusicBrainz API errors.
data MusicBrainzError
  = RateLimitExceeded
  | APITimeout
  | NoMatchFound
  | InvalidMBID Text
  | APIError Text  -- Generic API error
  deriving (Show, Eq, Generic)

-- | Convert error to human-readable text.
prettyError :: AppError -> Text
prettyError = \case
  DatabaseErr err -> case err of
    TrackNotFound tid -> "Track not found: " <> show tid
    ClusterNotFound cid -> "Cluster not found: " <> show cid
    DownloadNotFound did -> "Download not found: " <> show did
    ArtistNotFound aid -> "Artist not found: " <> show aid
    AlbumNotFound aid -> "Album not found: " <> show aid
    DuplicateKey key -> "Duplicate key: " <> key
    QueryFailed msg -> "Database query failed: " <> msg
    ConnectionFailed msg -> "Database connection failed: " <> msg
    MigrationFailed msg -> "Database migration failed: " <> msg
    TransactionFailed msg -> "Transaction failed: " <> msg

  ServiceErr err -> case err of
    ScanFailed msg -> "File system scan failed: " <> msg
    GroupingFailed msg -> "File grouping failed: " <> msg
    IdentificationFailed msg -> "Identification failed: " <> msg
    MetadataWriteFailed msg path -> "Failed to write metadata to " <> toText path <> ": " <> msg
    DownloadFailed msg -> "Download failed: " <> msg
    ImportFailed msg -> "Import failed: " <> msg
    ServiceCrashed msg -> "Service crashed: " <> msg

  ConfigErr err -> case err of
    ConfigNotFound path -> "Config file not found: " <> toText path
    ConfigParseFailed path msg -> "Failed to parse config " <> toText path <> ": " <> msg
    ConfigValidationFailed msg -> "Config validation failed: " <> msg
    InvalidLibraryPath path -> "Invalid library path: " <> path
    InvalidDatabaseConfig msg -> "Invalid database config: " <> msg

  MusicBrainzErr err -> case err of
    RateLimitExceeded -> "MusicBrainz rate limit exceeded"
    APITimeout -> "MusicBrainz API timeout"
    NoMatchFound -> "No MusicBrainz match found"
    InvalidMBID mbid -> "Invalid MBID: " <> mbid
    APIError msg -> "MusicBrainz API error: " <> msg

  UnexpectedError msg -> "Unexpected error: " <> msg

-- | Log an error with appropriate severity.
logError :: Katip m => AppError -> KatipContextT m ()
logError err = case err of
  DatabaseErr (ConnectionFailed _) -> $(logTM) ErrorS $ logStr $ prettyError err
  DatabaseErr _ -> $(logTM) WarningS $ logStr $ prettyError err
  ServiceErr (ServiceCrashed _) -> $(logTM) ErrorS $ logStr $ prettyError err
  ServiceErr _ -> $(logTM) WarningS $ logStr $ prettyError err
  ConfigErr _ -> $(logTM) ErrorS $ logStr $ prettyError err
  MusicBrainzErr RateLimitExceeded -> $(logTM) WarningS $ logStr $ prettyError err
  MusicBrainzErr NoMatchFound -> $(logTM) InfoS $ logStr $ prettyError err
  MusicBrainzErr _ -> $(logTM) WarningS $ logStr $ prettyError err
  UnexpectedError _ -> $(logTM) ErrorS $ logStr $ prettyError err

-- | Map the Left value of an Either.
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

-- | Convert Either to Maybe, discarding error.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

-- | Convert Maybe to Either with provided error.
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x
