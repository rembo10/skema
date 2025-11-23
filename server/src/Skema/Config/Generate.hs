{-# LANGUAGE OverloadedStrings #-}

-- | Generate configuration artifacts from Schema.
--
-- Usage:
--   writeConfigArtifacts   -- Write config.example.yaml
module Skema.Config.Generate
  ( writeConfigArtifacts
  ) where

import Skema.Config.Schema (generateFullYaml)
import qualified Data.Text.IO as TIO

-- | Write all generated artifacts to files.
writeConfigArtifacts :: IO ()
writeConfigArtifacts = do
  putStrLn "Generating configuration artifacts..."
  TIO.writeFile "config.example.yaml" generateFullYaml
  putStrLn "Generated: config.example.yaml"
