{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Version API handler.
module Skema.API.Handlers.Version
  ( versionServer
  , LatestRelease(..)
  , startUpdateChecker
  ) where

import Skema.API.Types.Version (VersionAPI, VersionResponse(..))
import Skema.Version (getVersionInfo, VersionInfo(..))
import qualified Skema.Config.Types as Cfg
import Servant
import Data.Aeson (FromJSON(..), withObject, (.:), decode)
import Network.HTTP.Client (Manager, parseRequest, httpLbs, responseBody, responseStatus, requestHeaders, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (HeaderName)
import Control.Exception (try)
import qualified Data.Text as T
import Katip
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)

-- | Cached latest release info.
data LatestRelease = LatestRelease
  { lrVersion :: Text
  , lrUpdateAvailable :: Bool
  } deriving (Show, Eq)

-- | GitHub release response (minimal).
data GitHubRelease = GitHubRelease
  { ghTagName :: Text
  } deriving (Show)

instance FromJSON GitHubRelease where
  parseJSON = withObject "GitHubRelease" $ \o ->
    GitHubRelease <$> o .: "tag_name"

-- | GitHub compare response (minimal).
-- We only need the "status" field: "ahead", "behind", "identical", or "diverged".
newtype GitHubCompare = GitHubCompare Text

instance FromJSON GitHubCompare where
  parseJSON = withObject "GitHubCompare" $ \o ->
    GitHubCompare <$> o .: "status"

-- | Version API handler.
versionServer :: TVar (Maybe LatestRelease) -> Server VersionAPI
versionServer latestVar = liftIO $ do
  vi <- getVersionInfo
  latest <- readTVarIO latestVar
  pure $ VersionResponse
    { vrVersion = viVersion vi
    , vrCommit = viCommit vi
    , vrChannel = viChannel vi
    , vrLatestVersion = lrVersion <$> latest
    , vrUpdateAvailable = maybe False lrUpdateAvailable latest
    }

-- | Check interval: 12 hours in microseconds.
checkIntervalMicros :: Int
checkIntervalMicros = 12 * 60 * 60 * 1000000

-- | GitHub API headers.
githubHeaders :: [(HeaderName, ByteString)]
githubHeaders = [("User-Agent", "Skema"), ("Accept", "application/vnd.github.v3+json")]

-- | Start the background update checker thread.
-- Checks GitHub releases API periodically and caches the result.
startUpdateChecker :: LogEnv -> TVar Cfg.Config -> TVar (Maybe LatestRelease) -> IO ()
startUpdateChecker le configVar latestVar = do
  vi <- getVersionInfo
  _ <- async $ forever $ do
    config <- readTVarIO configVar
    let checkEnabled = Cfg.systemCheckUpdates (Cfg.system config)
    if checkEnabled
      then do
        result <- checkForUpdate le vi
        case result of
          Just release -> atomically $ writeTVar latestVar (Just release)
          Nothing -> pure ()
      else
        atomically $ writeTVar latestVar Nothing
    threadDelay checkIntervalMicros
  pure ()

-- | Check for updates. For source channel builds with a known commit,
-- uses the GitHub compare API to avoid false positives when running
-- ahead of the latest release.
checkForUpdate :: LogEnv -> VersionInfo -> IO (Maybe LatestRelease)
checkForUpdate le vi = do
  manager <- newManager tlsManagerSettings
  fetchLatestRelease le manager >>= \case
    Nothing -> pure Nothing
    Just (GitHubRelease tag) -> do
      let latestVer = fromMaybe tag (T.stripPrefix "v" tag)
          currentVer = viVersion vi
          commit = viCommit vi
          ch = viChannel vi
          versionBehind = compareVersions latestVer currentVer

      -- For source builds with a known commit, check if we're ahead of
      -- the release tag. This prevents false "update available" when
      -- running from a git checkout past the latest release.
      if versionBehind && ch == "source" && commit /= "UNKNOWN"
        then do
          isAhead <- checkCommitAhead le manager tag commit
          let updateAvail = not isAhead
          logResult le currentVer latestVer updateAvail
                    (if isAhead then " (commit is ahead of release)" else "")
          pure $ Just $ LatestRelease latestVer updateAvail
        else do
          logResult le currentVer latestVer versionBehind ""
          pure $ Just $ LatestRelease latestVer versionBehind

-- | Log the update check result.
logResult :: LogEnv -> Text -> Text -> Bool -> Text -> IO ()
logResult le currentVer latestVer updateAvail extra =
  runKatipContextT le () "update-checker" $
    $(logTM) InfoS $ logStr $
      "Update check: current=" <> currentVer <> " latest=" <> latestVer <>
      (if updateAvail then " (update available)" else " (up to date)") <> extra

-- | Fetch the latest release from GitHub.
fetchLatestRelease :: LogEnv -> Manager -> IO (Maybe GitHubRelease)
fetchLatestRelease le manager = do
  let url = "https://api.github.com/repos/rembo10/skema/releases/latest"
  case parseRequest url of
    Nothing -> pure Nothing
    Just req -> do
      let req' = req { requestHeaders = githubHeaders }
      result <- try (httpLbs req' manager)
      case result of
        Left (err :: SomeException) -> do
          runKatipContextT le () "update-checker" $
            $(logTM) DebugS $ logStr $ ("Failed to check for updates: " <> show err :: Text)
          pure Nothing
        Right response -> do
          let status = statusCode (responseStatus response)
          if status == 200
            then case decode (responseBody response) of
              Just release -> pure $ Just release
              Nothing -> do
                runKatipContextT le () "update-checker" $
                  $(logTM) DebugS $ logStr ("Failed to parse GitHub release response" :: Text)
                pure Nothing
            else do
              when (status /= 404) $
                runKatipContextT le () "update-checker" $
                  $(logTM) DebugS $ logStr $ ("GitHub API returned status " <> show status :: Text)
              pure Nothing

-- | Check if the current commit is at or ahead of a release tag using
-- the GitHub compare API. Returns True if we're ahead or identical.
-- Falls back to False on any error (so version comparison takes over).
checkCommitAhead :: LogEnv -> Manager -> Text -> Text -> IO Bool
checkCommitAhead le manager tag commit = do
  let url = "https://api.github.com/repos/rembo10/skema/compare/"
            <> toString tag <> "..." <> toString commit
  case parseRequest url of
    Nothing -> pure False
    Just req -> do
      let req' = req { requestHeaders = githubHeaders }
      result <- try (httpLbs req' manager)
      case result of
        Left (_ :: SomeException) -> pure False
        Right response -> do
          let status = statusCode (responseStatus response)
          if status == 200
            then case decode (responseBody response) of
              Just (GitHubCompare compareStatus) -> do
                let ahead = compareStatus == "ahead" || compareStatus == "identical"
                runKatipContextT le () "update-checker" $
                  $(logTM) DebugS $ logStr $
                    ("Commit compare: " <> compareStatus <>
                     " (commit " <> T.take 7 commit <> " vs " <> tag <> ")" :: Text)
                pure ahead
              Nothing -> pure False
            else pure False

-- | Compare version strings to determine if an update is available.
-- Returns True if the latest version is newer than the current version.
compareVersions :: Text -> Text -> Bool
compareVersions latest current =
  let parseVer = map (fromMaybe 0 . readMaybe . toString) . T.splitOn "."
      latestParts = parseVer latest :: [Int]
      currentParts = parseVer current :: [Int]
      maxLen = max (length latestParts) (length currentParts)
      pad xs = xs ++ replicate (maxLen - length xs) 0
  in pad latestParts > pad currentParts
