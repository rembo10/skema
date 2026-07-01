{-# LANGUAGE OverloadedStrings #-}

-- | Quality Profiles API handlers.
module Skema.API.Handlers.QualityProfiles
  ( qualityProfilesServer
  ) where

import Skema.API.Types.QualityProfiles (QualityProfilesAPI, CreateQualityProfileRequest(..), UpdateQualityProfileRequest(..))
import Skema.API.Handlers.Utils (throw404, withDB)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Config.Types as Cfg
import Skema.Domain.Quality (QualityProfile(..))
import Servant

-- | Quality Profiles API handlers.
qualityProfilesServer :: Cfg.ServerConfig -> ConnectionPool -> Server QualityProfilesAPI
qualityProfilesServer _serverCfg connPool =
  getAllProfilesHandler
  :<|> getProfileHandler
  :<|> createProfileHandler
  :<|> updateProfileHandler
  :<|> deleteProfileHandler
  :<|> getDefaultProfileHandler
  :<|> setDefaultProfileHandler
  where
    getAllProfilesHandler :: Handler [QualityProfile]
    getAllProfilesHandler =
      withDB connPool DB.getAllQualityProfiles

    getProfileHandler :: Int64 -> Handler QualityProfile
    getProfileHandler profileId = do
      maybeProfile <- liftIO $ withConnection connPool $ \conn ->
        DB.getQualityProfile conn profileId
      case maybeProfile of
        Nothing -> throw404 $ "Quality profile not found: " <> show profileId
        Just profile -> pure profile

    createProfileHandler :: CreateQualityProfileRequest -> Handler QualityProfile
    createProfileHandler req = do
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

    updateProfileHandler :: Int64 -> UpdateQualityProfileRequest -> Handler QualityProfile
    updateProfileHandler profileId req = do
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

    deleteProfileHandler :: Int64 -> Handler NoContent
    deleteProfileHandler profileId = do
      withDB connPool $ \conn ->
        DB.deleteQualityProfile conn profileId
      pure NoContent

    getDefaultProfileHandler :: Handler (Maybe QualityProfile)
    getDefaultProfileHandler = do
      maybeProfileId <- liftIO $ withConnection connPool $ \conn ->
        DB.getDefaultQualityProfileId conn
      case maybeProfileId of
        Nothing -> pure Nothing
        Just profileId -> liftIO $ withConnection connPool $ \conn ->
          DB.getQualityProfile conn profileId

    setDefaultProfileHandler :: Int64 -> Handler NoContent
    setDefaultProfileHandler profileId = do
      withDB connPool $ \conn ->
        DB.setDefaultQualityProfileId conn (Just profileId)
      pure NoContent
