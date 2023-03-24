-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.M
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Metadata (m)"
    [ metadata args,
      empty args
    ]

metadata :: IO FilePath -> TestTree
metadata args = testCase "Prints metadata" $ do
  testDir <- getTestPath args "metadata"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- METADATA

  let metaArgList = ["m", "-t", trashDir]
  (metadataResult, _) <- captureSafeRmLogs metaArgList

  -- assert nothing changed
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  assertMatches expectedMetadata metadataResult

  -- trash structure assertions
  (metadataIdxSet, metadatMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet metadataIdxSet
  delExpectedMetadata @=? metadatMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/m/metadata/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/m/metadata/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/m/metadata/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/m/metadata/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/m/metadata/dir2"
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

empty :: IO FilePath -> TestTree
empty args = testCase "Prints empty metadata" $ do
  testDir <- getTestPath args "empty"
  let trashDir = testDir </> ".trash"

  createDirectories [testDir, trashDir, trashDir </> "info", trashDir </> "paths"]
  createFiles [trashDir </> "log"]

  let metaArgList = ["m", "-t", trashDir]
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
getTestPath mroot = createTestDir mroot "m"
