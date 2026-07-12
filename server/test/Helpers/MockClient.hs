{-# LANGUAGE OverloadedStrings #-}

-- | Build a standalone mock 'HttpClient' for tests that only need HTTP.
--
-- Unlike 'Helpers.TestEnv', this pulls in no database, event bus, or service
-- context — just an 'HttpClient' whose transport answers from a fixed list of
-- 'RequestMatcher's. Used by the download-client specs to drive each client's
-- 'DownloadClientAPI' methods against canned JSON.
module Helpers.MockClient
  ( withMockHttpClient
  ) where

import Control.Exception (bracket)
import Katip
  ( LogEnv
  , Severity(ErrorS)
  , Verbosity(V0)
  , closeScribes
  , defaultScribeSettings
  , initLogEnv
  , permitItem
  , mkHandleScribe
  , registerScribe
  , ColorStrategy(ColorIfTerminal)
  )
import System.IO (stderr)

import Skema.HTTP.Client
  ( HttpClient
  , newHttpClientWithTransport
  , defaultHttpConfig
  , defaultUserAgentData
  )
import Helpers.MockHttp (RequestMatcher, mockTransport)

-- | Run an action with a mock 'HttpClient' whose transport answers from the
-- given matchers. Unmatched requests throw (see 'mockTransport'). The log env
-- only surfaces errors and above to keep test output quiet.
withMockHttpClient :: [RequestMatcher] -> (HttpClient -> IO a) -> IO a
withMockHttpClient matchers action =
  bracket mkLogEnv closeScribes $ \le -> do
    client <- newHttpClientWithTransport le defaultHttpConfig defaultUserAgentData
                (mockTransport matchers)
    action client
  where
    mkLogEnv :: IO LogEnv
    mkLogEnv = do
      scribe <- mkHandleScribe ColorIfTerminal stderr (permitItem ErrorS) V0
      registerScribe "stderr" scribe defaultScribeSettings =<< initLogEnv "skema-test" "test"
