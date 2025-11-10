{-# LANGUAGE OverloadedStrings #-}

-- | Cryptography adapter for password hashing.
--
-- This module wraps IO-based crypto operations.
-- It's kept separate from domain logic to maintain purity.
module Skema.Adapters.Crypto
  ( -- * Password hashing
    hashPasswordIfNeeded
  , isHashedPassword
  ) where

import qualified Skema.Config.Types as Cfg

-- | Hash a password if it's not already hashed.
--
-- This checks if the password is already hashed and only hashes plain text.
-- Returns Nothing if hashing fails.
hashPasswordIfNeeded :: Text -> IO (Maybe Text)
hashPasswordIfNeeded plaintext = do
  if isHashedPassword plaintext
    then pure (Just plaintext)  -- Already hashed
    else Cfg.hashPassword plaintext  -- Hash it

-- | Check if a password is already hashed.
isHashedPassword :: Text -> Bool
isHashedPassword = Cfg.isHashedPassword
