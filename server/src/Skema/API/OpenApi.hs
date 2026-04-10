{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | OpenAPI specification generation and Swagger UI.
module Skema.API.OpenApi
  ( openApiSpec
  , swaggerUiHtml
  ) where

import Skema.API.Types (DocumentedAPI)
import Data.OpenApi (OpenApi, info, title, version, description)
import Control.Lens ((.~), (?~))
import Servant ((:>))
import Servant.API.EventStream (ServerSentEvents)
import Servant.OpenApi (HasOpenApi(..))

-- | Orphan instance for ServerSentEvents since servant-event-stream
-- does not depend on openapi3.
instance HasOpenApi (ServerSentEvents a) where
  toOpenApi _ = mempty

-- | The generated OpenAPI specification.
openApiSpec :: OpenApi
openApiSpec = toOpenApi (Proxy :: Proxy ("api" :> DocumentedAPI))
  & info . title .~ "Skema API"
  & info . version .~ "0.1.0"
  & info . description ?~ "Music library management and acquisition system"

-- | Swagger UI HTML page that loads the OpenAPI spec.
swaggerUiHtml :: ByteString
swaggerUiHtml = encodeUtf8 (mconcat
  [ "<!DOCTYPE html>\n"
  , "<html lang=\"en\">\n"
  , "<head>\n"
  , "  <meta charset=\"utf-8\">\n"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
  , "  <title>Skema API</title>\n"
  , "  <link rel=\"stylesheet\" href=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui.css\">\n"
  , "  <style>html { box-sizing: border-box; } *, *:before, *:after { box-sizing: inherit; } body { margin: 0; }</style>\n"
  , "</head>\n"
  , "<body>\n"
  , "  <div id=\"swagger-ui\"></div>\n"
  , "  <script src=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js\"></script>\n"
  , "  <script>\n"
  , "    SwaggerUIBundle({\n"
  , "      url: './docs/openapi.json',\n"
  , "      dom_id: '#swagger-ui',\n"
  , "      presets: [SwaggerUIBundle.presets.apis, SwaggerUIBundle.SwaggerUIStandalonePreset],\n"
  , "      layout: 'BaseLayout'\n"
  , "    });\n"
  , "  </script>\n"
  , "</body>\n"
  , "</html>\n"
  ] :: Text)
