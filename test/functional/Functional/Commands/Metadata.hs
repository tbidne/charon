-- | Tests for m command.
module Functional.Commands.Metadata
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Metadata Command"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    [ metadata backend args,
      empty backend args
    ]

metadata :: Backend -> IO FilePath -> TestTree
metadata backend args = testCase "Prints metadata" $ do
  testDir <- getTestPath args (withBackendDir backend "metadata")
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = withSrArgs trashDir backend ("delete" : filesToDelete <> dirsToDelete)

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- METADATA

  let metaArgList = withSrArgs trashDir backend ["metadata"]
  (metadataResult, _) <- captureSafeRmLogs metaArgList

  -- assert nothing changed
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  assertMatches expectedMetadata metadataResult

  -- trash structure assertions
  (metadataIdxSet, metadatMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet metadataIdxSet
  delExpectedMetadata @=? metadatMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "metadata/metadata" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "metadata/metadata" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "metadata/metadata" "f3"),
          mkPathData' backend PathTypeDirectory "dir1" (withBackendBaseDir backend "metadata/metadata" "dir1"),
          mkPathData' backend PathTypeDirectory "dir2" (withBackendBaseDir backend "metadata/metadata" "dir2")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    expectedMetadata =
      Exact
        <$> [ "Entries:      5",
              "Total Files:  4",
              "Log size:     0.00B",
              "Size:         0.00B",
              ""
            ]

empty :: Backend -> IO FilePath -> TestTree
empty backend args = testCase "Prints empty metadata" $ do
  testDir <- getTestPath args (withBackendDir backend "empty")
  let trashDir = testDir </> ".trash"

  createDirectories [testDir, trashDir, trashDir </> "info", trashDir </> "files"]
  createFiles [trashDir </> "log"]

  let metaArgList = withSrArgs trashDir backend ["metadata"]
  (result, _) <- captureSafeRmLogs metaArgList

  assertMatches expectedTerminal result
  where
    expectedTerminal =
      Exact
        <$> [ "Entries:      0",
              "Total Files:  0",
              "Log size:     5.00B",
              "Size:         0.00B",
              ""
            ]

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "metadata"
