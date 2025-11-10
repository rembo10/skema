{-# LANGUAGE OverloadedStrings #-}

-- | Path expansion utilities for config file paths.
--
-- This module provides utilities to expand tilde (~) and environment variables
-- in file paths, following standard shell conventions.
module Skema.Config.PathExpansion
  ( expandPath
  , expandPathIO
  ) where

import qualified Data.Text as T
import System.Directory (getHomeDirectory)
import Data.Char (isAlphaNum)

-- | Expand a path with tilde and environment variables.
--
-- This is the IO version that performs actual lookups.
--
-- Supports:
-- - Tilde expansion: ~ or ~/path -> /home/user or /home/user/path
-- - Environment variables: $VAR or ${VAR} -> value from environment
--
-- Examples:
-- - "~/music" -> "/home/user/music"
-- - "$HOME/music" -> "/home/user/music"
-- - "${XDG_DATA_HOME}/skema" -> "/home/user/.local/share/skema"
-- - "~/$USER/files" -> "/home/user/username/files"
expandPathIO :: Text -> IO Text
expandPathIO path = do
  -- First expand tilde
  pathWithTilde <- expandTilde path
  -- Then expand environment variables
  expandEnvVars pathWithTilde

-- | Pure version of expandPath (for non-IO contexts).
--
-- Note: This version cannot expand ~ or environment variables,
-- so it just returns the path as-is. Use expandPathIO for actual expansion.
expandPath :: Text -> Text
expandPath = identity

-- | Expand tilde at the start of a path.
expandTilde :: Text -> IO Text
expandTilde path
  | "~/" `T.isPrefixOf` path = do
      home <- getHomeDirectory
      pure $ toText home <> T.drop 1 path
  | path == "~" = do
      home <- getHomeDirectory
      pure $ toText home
  | otherwise = pure path

-- | Expand environment variables in a path.
--
-- Supports both $VAR and ${VAR} syntax.
expandEnvVars :: Text -> IO Text
expandEnvVars = expandEnvVarsHelper

expandEnvVarsHelper :: Text -> IO Text
expandEnvVarsHelper text = do
  case T.findIndex (== '$') text of
    Nothing -> pure text  -- No variables to expand
    Just idx ->
      let (before, rest) = T.splitAt idx text
      in if T.null rest
           then pure text  -- $ at end of string
           else do
             let afterDollar = T.tail rest
             case parseVarName afterDollar of
               Nothing -> do
                 -- Invalid variable syntax, skip this $ and continue
                 remaining <- expandEnvVarsHelper (T.tail rest)
                 pure $ before <> "$" <> remaining
               Just (varName, afterVar) -> do
                 -- Look up the variable
                 maybeValue <- lookupEnv (toString varName)
                 let value = maybe "" toText maybeValue
                 -- Continue expanding in the remaining text
                 remaining <- expandEnvVarsHelper afterVar
                 pure $ before <> value <> remaining

-- | Parse a variable name from the start of text.
--
-- Handles both $VAR and ${VAR} syntax.
-- Returns Just (varName, remainingText) or Nothing if invalid.
parseVarName :: Text -> Maybe (Text, Text)
parseVarName text
  | T.null text = Nothing
  | T.head text == '{' =
      -- ${VAR} syntax
      case T.findIndex (== '}') text of
        Nothing -> Nothing  -- Unclosed brace
        Just endIdx ->
          let varName = T.take (endIdx - 1) (T.tail text)
              remaining = T.drop (endIdx + 1) text
          in if T.null varName || not (isValidVarName varName)
               then Nothing
               else Just (varName, remaining)
  | otherwise =
      -- $VAR syntax - take alphanumeric and underscore
      let varName = T.takeWhile isVarChar text
          remaining = T.drop (T.length varName) text
      in if T.null varName
           then Nothing
           else Just (varName, remaining)

-- | Check if a character is valid in a variable name.
isVarChar :: Char -> Bool
isVarChar c = isAlphaNum c || c == '_'

-- | Check if a text is a valid variable name.
isValidVarName :: Text -> Bool
isValidVarName name = not (T.null name) && T.all isVarChar name
