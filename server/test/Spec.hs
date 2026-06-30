{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import qualified Integration.GrouperSpec
import qualified Integration.ScanGroupIdentifySpec
import qualified Integration.EndToEndSpec
import qualified Integration.SubmissionSpec
import qualified Integration.QualityProfilesHandlerSpec
import qualified Integration.DownloadsRepoSpec
import qualified Integration.CatalogAlbumRepoSpec
import qualified Integration.CatalogArtistRepoSpec
import qualified Integration.QualityRepoSpec
import qualified Integration.ClustersRepoSpec
import qualified Integration.TracksRepoSpec
import qualified Unit.ConfigSpec
import qualified Unit.IndexerSpec
import qualified Unit.MatchingSpec
import qualified Unit.MusicBrainzSpec
import qualified Unit.Domain.PathFormatterSpec
import qualified Unit.Domain.CatalogSpec
import qualified Unit.Domain.QualitySpec
import qualified Unit.Domain.MetadataSpec
import qualified Unit.Domain.ImportSpec
import qualified Unit.Domain.ScoringSpec
import qualified Unit.Domain.AcquisitionSpec
import qualified Unit.Domain.IdentificationSpec
import qualified Unit.Domain.DownloadSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Skema Tests"
  [ Unit.ConfigSpec.tests
  , Unit.IndexerSpec.tests
  , Unit.MatchingSpec.tests
  , Unit.MusicBrainzSpec.tests
  , Unit.Domain.PathFormatterSpec.tests
  , Unit.Domain.CatalogSpec.tests
  , Unit.Domain.QualitySpec.tests
  , Unit.Domain.MetadataSpec.tests
  , Unit.Domain.ImportSpec.tests
  , Unit.Domain.ScoringSpec.tests
  , Unit.Domain.AcquisitionSpec.tests
  , Unit.Domain.IdentificationSpec.tests
  , Unit.Domain.DownloadSpec.tests
  , Integration.GrouperSpec.tests
  , Integration.ScanGroupIdentifySpec.tests
  , Integration.EndToEndSpec.tests
  , Integration.SubmissionSpec.tests
  , Integration.QualityProfilesHandlerSpec.tests
  , Integration.DownloadsRepoSpec.tests
  , Integration.CatalogAlbumRepoSpec.tests
  , Integration.CatalogArtistRepoSpec.tests
  , Integration.QualityRepoSpec.tests
  , Integration.ClustersRepoSpec.tests
  , Integration.TracksRepoSpec.tests
  ]
