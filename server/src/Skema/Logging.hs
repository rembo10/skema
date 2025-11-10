{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Logging configuration and utilities.
--
-- This module provides a custom Katip-based logging setup with:
-- - Colored output for different log levels and modules
-- - Aligned formatting for easy reading
-- - Concise format without unnecessary metadata
-- - Stdout only (use systemd/docker/logrotate for file management)
module Skema.Logging
  ( initializeLogging
  , customLogFormatter
  , withKatipNamespace
  , withKatipContext
  ) where

import Katip
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Char ()
import System.IO ()

-- | ANSI color codes for terminal output
data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

-- | Custom log formatter: [timestamp] [level] [module] message
customLogFormatter :: ItemFormatter a
customLogFormatter withColor _verb Item{..} =
  let
    -- Format timestamp
    timestampStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" _itemTime

    -- Strip "skema." prefix from namespace
    nsText = T.intercalate "." $ unNamespace _itemNamespace
    moduleStr = fromMaybe nsText $ T.stripPrefix "skema." nsText

    -- Pad module name to 10 characters for alignment
    paddedModuleStr = T.justifyLeft 8 ' ' moduleStr

    -- Color module name based on hash of module string
    coloredModuleStr = colorize withColor (moduleColor moduleStr) paddedModuleStr

    -- Format severity with color (padded to 9 characters for alignment)
    levelFmt c t = colorize withColor c (T.justifyLeft 5 ' ' t)
    severityStr = case _itemSeverity of
      DebugS     -> levelFmt Magenta "DEBUG"
      InfoS      -> levelFmt Green "INFO"
      NoticeS    -> levelFmt Cyan "NOTE"
      WarningS   -> levelFmt Yellow "WARN"
      ErrorS     -> levelFmt Red "ERROR"
      CriticalS  -> levelFmt Red "CRIT"
      AlertS     -> levelFmt Red "ALERT"
      EmergencyS -> levelFmt Red "EMERG"

    -- Build final log line using Text.Lazy.Builder (no extra newline)
    -- Format: [timestamp] [level] [module] message
  in
    TLB.fromText "[" <> TLB.fromString timestampStr <> TLB.fromText "] ["
      <> TLB.fromText severityStr <> TLB.fromText "] ["
      <> TLB.fromText coloredModuleStr <> TLB.fromText "] "
      <> unLogStr _itemMessage
  where
    -- Assign a color to a module name (manually for known modules, hash for others)
    moduleColor :: Text -> Color
    moduleColor modName = case modName of
      "main" -> Blue
      "database" -> Magenta
      "server" -> Cyan
      "config" -> Yellow
      "scanner" -> Green
      "watcher" -> Magenta
      _ ->
        -- For unknown modules, use a better hash
        let colors = [Cyan, Blue, Magenta, Yellow, Green]
            -- Use length + first char + last char for better distribution
            hash = T.length modName * 17 +
                   maybe 0 ord (viaNonEmpty head $ toString modName) * 31 +
                   maybe 0 ord (viaNonEmpty last $ toString modName) * 13
            idx = hash `mod` length colors
        in fromMaybe Cyan (colors !!? idx)

    colorize :: Bool -> Color -> Text -> Text
    colorize False _ txt = txt
    colorize True color txt = colorBracket color <> txt <> resetColor

    colorBracket :: Color -> Text
    colorBracket color = "\ESC[" <> colorCode color <> "m"

    resetColor :: Text
    resetColor = "\ESC[0m"

    colorCode :: Color -> Text
    colorCode Black   = "30"
    colorCode Red     = "31"
    colorCode Green   = "32"
    colorCode Yellow  = "33"
    colorCode Blue    = "34"
    colorCode Magenta = "35"
    colorCode Cyan    = "36"
    colorCode White   = "37"

-- | Initialize Katip logging environment (stdout only).
--
-- For file logging, use:
-- - systemd: Automatic via journalctl
-- - Docker: Automatic via docker logs
-- - Manual: Redirect stdout (skema > /var/log/skema.log)
-- - logrotate: Configure for rotated file logs
initializeLogging :: IO LogEnv
initializeLogging = do
  handleScribe <- mkHandleScribeWithFormatter customLogFormatter ColorIfTerminal stdout (permitItem InfoS) V0
  registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "skema" "production"

-- | Helper to run an action with a Katip namespace context.
-- Reduces boilerplate by using empty initial context.
withKatipNamespace :: LogEnv -> Text -> KatipContextT IO a -> IO a
withKatipNamespace le namespace action =
  runKatipContextT le () (fromString $ toString namespace) action

-- | Helper to run an action with a Katip namespace and additional context.
-- Useful for adding structured context like task IDs, file paths, etc.
withKatipContext :: (LogItem context) => LogEnv -> Text -> context -> KatipContextT IO a -> IO a
withKatipContext le namespace ctx action =
  runKatipContextT le ctx (fromString $ toString namespace) action
