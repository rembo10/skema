{-# LANGUAGE OverloadedStrings #-}

-- | Architecture invariants enforced as tests.
--
-- The domain layer must stay pure and independent: modules under
-- @Skema.Domain@ may not import from the database, service, or API layers.
-- This keeps domain logic testable without IO and prevents the dependency
-- direction from silently inverting again.
module Unit.ArchitectureSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory (listDirectory)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Import prefixes that domain modules must never use.
forbiddenImports :: [Text]
forbiddenImports =
  [ "import Skema.Database"
  , "import Skema.Services"
  , "import Skema.API"
  ]

domainDir :: FilePath
domainDir = "src/Skema/Domain"

tests :: TestTree
tests = testGroup "Unit.Architecture"
  [ testCase "Skema.Domain modules do not import database/service/API layers" $ do
      files <- sort . filter (".hs" `isSuffixOf`) <$> listDirectory domainDir
      assertBool "expected domain modules to exist" (not (null files))
      violations <- fmap concat $ forM files $ \file -> do
        contents <- TIO.readFile (domainDir <> "/" <> file)
        let offending = filter isForbidden (T.lines contents)
        pure [(file, line) | line <- offending]
      unless (null violations) $
        assertFailure $ toString $ T.unlines $
          "domain modules must not depend on outer layers:"
            : [toText file <> ": " <> line | (file, line) <- violations]
  ]
  where
    isForbidden line =
      any (`T.isPrefixOf` T.stripStart line) forbiddenImports
