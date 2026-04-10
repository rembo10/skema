{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level API type definition.
--
-- This module defines the complete API by composing all endpoint sub-APIs.
-- Individual endpoint types are defined in specialized sub-modules:
--
-- * "Skema.API.Types.Auth" - Authentication endpoints
-- * "Skema.API.Types.Library" - Library management endpoints
-- * "Skema.API.Types.Config" - Configuration endpoints
-- * "Skema.API.Types.Events" - Server-Sent Events endpoints
-- * "Skema.API.Types.Diffs" - Metadata diffs endpoints
-- * "Skema.API.Types.Clusters" - Album cluster endpoints
-- * "Skema.API.Types.Acquisition" - Acquisition rules endpoints
-- * "Skema.API.Types.Catalog" - Artist/album catalog endpoints
-- * "Skema.API.Types.Downloads" - Download management endpoints
-- * "Skema.API.Types.Filesystem" - Filesystem browsing endpoints
-- * "Skema.API.Types.QualityProfiles" - Quality profile endpoints
-- * "Skema.API.Types.Tasks" - Task management endpoints
-- * "Skema.API.Types.Common" - Shared common types
module Skema.API.Types
  ( API
  , DocumentedAPI
  ) where

import Skema.API.Types.Auth (AuthAPI)
import Skema.API.Types.Library (LibraryAPI, StatsAPI)
import Skema.API.Types.Config (ConfigAPI, ConfigSchemaAPI)
import Skema.API.Types.Diffs (DiffsAPI)
import Skema.API.Types.Clusters (ClustersAPI)
import Skema.API.Types.Acquisition (AcquisitionAPI)
import Skema.API.Types.Catalog (CatalogAPI)
import Skema.API.Types.Downloads (DownloadsAPI)
import Skema.API.Types.Events (EventsAPI)
import Skema.API.Types.Filesystem (FilesystemAPI)
import Skema.API.Types.QualityProfiles (QualityProfilesAPI)
import Skema.API.Types.Tasks (TasksAPI)
import Skema.API.Types.Version (VersionAPI)
import Skema.API.Types.Common (RawHtml)
import Data.OpenApi (OpenApi)
import Servant

-- | The documented portion of the API (for OpenAPI spec generation).
type DocumentedAPI = AuthAPI :<|> LibraryAPI :<|> ConfigAPI :<|> ConfigSchemaAPI :<|> DiffsAPI :<|> ClustersAPI :<|> StatsAPI :<|> AcquisitionAPI :<|> CatalogAPI :<|> DownloadsAPI :<|> EventsAPI :<|> FilesystemAPI :<|> QualityProfilesAPI :<|> TasksAPI

-- | Docs endpoints: Swagger UI and OpenAPI spec.
type DocsAPI = "docs" :>
  ( Get '[RawHtml] ByteString
  :<|> "openapi.json" :> Get '[JSON] OpenApi
  )

-- | Top-level API combining all sub-APIs and static file serving.
type API = "api" :> (DocumentedAPI :<|> VersionAPI :<|> DocsAPI)
      :<|> "images" :> Raw
      :<|> Raw  -- Catch-all for frontend SPA
