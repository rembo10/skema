{-# LANGUAGE OverloadedStrings #-}

-- | Quality Profiles API handlers.
module Skema.API.Handlers.QualityProfiles
  ( qualityProfilesServer
  ) where

import Skema.API.Types.QualityProfiles (QualityProfilesAPI, CreateQualityProfileRequest(..), UpdateQualityProfileRequest(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Config.Types as Cfg
import Skema.Domain.Quality (QualityProfile(..))
import Servant

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

-- | Quality Profiles API handlers.
qualityProfilesServer :: Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> TVar Cfg.Config -> Server QualityProfilesAPI
qualityProfilesServer _serverCfg jwtSecret connPool configVar = \maybeAuthHeader ->
  getAllProfilesHandler maybeAuthHeader
  :<|> getProfileHandler maybeAuthHeader
  :<|> createProfileHandler maybeAuthHeader
  :<|> updateProfileHandler maybeAuthHeader
  :<|> deleteProfileHandler maybeAuthHeader
  :<|> getDefaultProfileHandler maybeAuthHeader
  :<|> setDefaultProfileHandler maybeAuthHeader
  where
    getAllProfilesHandler :: Maybe Text -> Handler [QualityProfile]
    getAllProfilesHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.getAllQualityProfiles conn

    getProfileHandler :: Maybe Text -> Int64 -> Handler QualityProfile
    getProfileHandler authHeader profileId = do
      _ <- requireAuth configVar jwtSecret authHeader
      maybeProfile <- liftIO $ withConnection connPool $ \conn ->
        DB.getQualityProfile conn profileId
      case maybeProfile of
        Nothing -> throw404 $ "Quality profile not found: " <> show profileId
        Just profile -> pure profile

    createProfileHandler :: Maybe Text -> CreateQualityProfileRequest -> Handler QualityProfile
    createProfileHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Create profile in database
      newId <- liftIO $ withConnection connPool $ \conn ->
        DB.insertQualityProfile
          conn
          (createQualityProfileName req)
          (createQualityProfileCutoffQuality req)
          (createQualityProfileQualityPreferences req)
          (createQualityProfileUpgradeAutomatically req)

      -- Return the created profile
      pure $ QualityProfile
        { qfId = Just newId
        , qfName = createQualityProfileName req
        , qfQualityPreferences = createQualityProfileQualityPreferences req
        , qfCutoffQuality = createQualityProfileCutoffQuality req
        , qfUpgradeAutomatically = createQualityProfileUpgradeAutomatically req
        }

    updateProfileHandler :: Maybe Text -> Int64 -> UpdateQualityProfileRequest -> Handler QualityProfile
    updateProfileHandler authHeader profileId req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Update profile in database
      liftIO $ withConnection connPool $ \conn ->
        DB.updateQualityProfile
          conn
          profileId
          (updateQualityProfileName req)
          (updateQualityProfileCutoffQuality req)
          (updateQualityProfileQualityPreferences req)
          (updateQualityProfileUpgradeAutomatically req)

      -- Return the updated profile
      pure $ QualityProfile
        { qfId = Just profileId
        , qfName = updateQualityProfileName req
        , qfQualityPreferences = updateQualityProfileQualityPreferences req
        , qfCutoffQuality = updateQualityProfileCutoffQuality req
        , qfUpgradeAutomatically = updateQualityProfileUpgradeAutomatically req
        }

    deleteProfileHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteProfileHandler authHeader profileId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteQualityProfile conn profileId
      pure NoContent

    getDefaultProfileHandler :: Maybe Text -> Handler (Maybe QualityProfile)
    getDefaultProfileHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      maybeProfileId <- liftIO $ withConnection connPool $ \conn ->
        DB.getDefaultQualityProfileId conn
      case maybeProfileId of
        Nothing -> pure Nothing
        Just profileId -> liftIO $ withConnection connPool $ \conn ->
          DB.getQualityProfile conn profileId

    setDefaultProfileHandler :: Maybe Text -> Int64 -> Handler NoContent
    setDefaultProfileHandler authHeader profileId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.setDefaultQualityProfileId conn (Just profileId)
      pure NoContent
