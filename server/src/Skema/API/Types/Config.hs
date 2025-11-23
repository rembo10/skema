{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Configuration API types.
--
-- The config API uses JSON Value directly:
-- - GET returns the full config JSON with computed fields (auth_enabled)
-- - PUT accepts partial JSON for merge-based updates
-- - GET /schema returns field metadata for UI generation
--
-- This eliminates the need for manually duplicated ConfigResponse/ConfigUpdate types.
-- The Config type's ToJSON is used directly, with computed fields added at runtime.
module Skema.API.Types.Config
  ( ConfigAPI
  , ConfigSchemaAPI
  ) where

import Data.Aeson (Value)
import Servant

-- | Configuration endpoints.
--
-- GET /config - Returns full config JSON with computed fields
-- PUT /config - Accepts partial JSON for merge-based updates
type ConfigAPI = "config" :> Header "Authorization" Text :>
  ( Get '[JSON] Value
  :<|> ReqBody '[JSON] Value :> Put '[JSON] Value
  )

-- | Config schema endpoint for UI generation.
--
-- GET /config/schema - Returns field metadata for dynamic UI
type ConfigSchemaAPI = "config" :> "schema" :> Get '[JSON] Value
