{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | JWT authentication for Skema.
--
-- Provides functions to generate and validate JSON Web Tokens for user authentication.
module Skema.Auth.JWT
  ( -- * JWT Generation
    generateJWT
  , JWTClaims(..)
    -- * JWT Validation
  , validateJWT
    -- * JWT Secret Management
  , JWTSecret
  , getJWTSecret
  , generateJWTSecretString
  ) where

import Crypto.JOSE (JWK, runJOSE, makeJWSHeader, fromOctets)
import Crypto.JWT (SignedJWT, JWTError(..), NumericDate(..), signClaims, defaultJWTValidationSettings, emptyClaimsSet, verifyClaims, string, claimIat, claimExp, claimSub, decodeCompact, encodeCompact)
import Control.Monad.Except (throwError)
import Control.Lens ((?~), (^.), preview, _Just, review)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Skema.Config.Types (ServerConfig(..))
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Random as Random

-- | JWT secret key for signing and validating tokens.
newtype JWTSecret = JWTSecret JWK
  deriving (Show)

-- | Claims embedded in the JWT.
data JWTClaims = JWTClaims
  { jwtUsername :: Text
  , jwtIssuedAt :: UTCTime
  , jwtExpiresAt :: UTCTime
  } deriving (Show, Eq)

-- | Get JWT secret from server config.
-- If the config has a JWT secret, it's decoded from hex and used.
-- If not, returns Nothing and the caller should generate and persist one.
getJWTSecret :: ServerConfig -> IO (Maybe JWTSecret)
getJWTSecret config = do
  case serverJwtSecret config of
    Nothing -> pure Nothing
    Just secretHex -> do
      -- Decode hex string to bytes
      case Base16.decode $ TE.encodeUtf8 secretHex of
        Left _err -> pure Nothing  -- Invalid hex in config
        Right secretBytes -> do
          -- Create JWK from bytes (256 bits = 32 bytes)
          pure $ Just (JWTSecret (fromOctets secretBytes))

-- | Generate a new random JWT secret as a hex string.
-- This can be saved to the config file.
generateJWTSecretString :: IO Text
generateJWTSecretString = do
  -- Generate 32 random bytes (256 bits)
  randomBytes <- Random.getRandomBytes 32 :: IO BS.ByteString
  -- Encode as hex
  pure $ decodeUtf8 $ Base16.encode randomBytes

-- | Generate a JWT for a user with configurable expiration.
generateJWT :: JWTSecret -> Text -> Int -> IO (Either JWTError (Text, UTCTime))
generateJWT (JWTSecret jwk) username expirationHours = do
  now <- getCurrentTime
  let expiresAt = addUTCTime (fromIntegral expirationHours * 60 * 60) now
      iat = NumericDate now
      exp' = NumericDate expiresAt

  -- Create claims set
  let claims = emptyClaimsSet
        & claimIat ?~ iat
        & claimExp ?~ exp'
        & claimSub ?~ review string username

  -- Sign the JWT using runJOSE monad
  result <- runJOSE $ do
    header <- makeJWSHeader jwk
    signedJWT <- signClaims jwk header claims
    pure $ decodeUtf8 $ BSL.toStrict $ encodeCompact signedJWT

  pure $ fmap (\token -> (token, expiresAt)) result

-- | Validate a JWT and extract the claims.
validateJWT :: JWTSecret -> Text -> IO (Either JWTError JWTClaims)
validateJWT (JWTSecret jwk) token = do
  let tokenBS = BSL.fromStrict $ TE.encodeUtf8 token

  -- Decode and verify the JWT using runJOSE monad
  result <- runJOSE $ do
    signedJWT <- decodeCompact @SignedJWT tokenBS
    claims <- verifyClaims (defaultJWTValidationSettings (const True)) jwk signedJWT

    -- Extract custom claims
    let username = preview (claimSub . _Just . string) claims
        iat = claims ^. claimIat
        exp' = claims ^. claimExp

    case (username, iat, exp') of
      (Just uname, Just (NumericDate issuedAt), Just (NumericDate expiresAt)) ->
        pure $ JWTClaims
          { jwtUsername = uname
          , jwtIssuedAt = issuedAt
          , jwtExpiresAt = expiresAt
          }
      _ -> throwError $ JWTClaimsSetDecodeError "Missing required claims"

  pure result
