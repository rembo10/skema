{-# LANGUAGE OverloadedStrings #-}

-- | Generate configuration artifacts from Schema.
--
-- Usage:
--   writeConfigArtifacts   -- Write config.example.yaml, TypeScript types, React components
module Skema.Config.Generate
  ( writeConfigArtifacts
  ) where

import Skema.Config.Schema (generateFullYaml, generateTypeScriptTypes, generateReactComponents)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)

-- | Write all generated artifacts to files.
writeConfigArtifacts :: IO ()
writeConfigArtifacts = do
  putStrLn "Generating configuration artifacts..."

  -- Generate example YAML config
  TIO.writeFile "../config.example.yaml" generateFullYaml
  putStrLn "  Generated: ../config.example.yaml"

  -- Generate TypeScript types
  createDirectoryIfMissing True "../web/src/types"
  TIO.writeFile "../web/src/types/config.generated.ts" generateTypeScriptTypes
  putStrLn "  Generated: ../web/src/types/config.generated.ts"

  -- Generate React components
  createDirectoryIfMissing True "../web/src/components"
  TIO.writeFile "../web/src/components/ConfigFields.generated.tsx" generateReactComponents
  putStrLn "  Generated: ../web/src/components/ConfigFields.generated.tsx"

  putStrLn "Done!"
