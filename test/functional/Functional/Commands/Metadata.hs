-- | Tests for m command.
module Functional.Commands.Metadata
  ( tests,
  )
where

import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Metadata Command"
    [ metadata testEnv',
      empty testEnv'
    ]
  where
    testEnv' = appendTestDir "metadata" <$> testEnv

metadata :: IO TestEnv -> TestTree
metadata getTestEnv = testCase "Prints metadata" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "metadata" $ do
    testDir <- getTestDir

    let filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
    delArgList <- withSrArgsM ("delete" : filesToDelete <> dirsToDelete)

    -- setup
    clearDirectory testDir
    -- test w/ a nested dir
    createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
    assertPathsExist (filesToDelete ++ dirsToDelete)

    runSafeRm delArgList

    -- file assertions
    delTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir1", "dir2"]
    assertPathsExist delTrashPaths
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "dir1",
          "dir2"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- METADATA

    metaArgList <- withSrArgsM ["metadata"]
    (metadataResult, _) <- captureSafeRmLogs metaArgList

    -- assert nothing changed
    assertPathsExist delTrashPaths
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    assertMatches expectedMetadata metadataResult

    -- trash structure assertions
    (metadataIdxSet, metadatMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet metadataIdxSet
    delExpectedMetadata @=? metadatMetadata
  where
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

empty :: IO TestEnv -> TestTree
empty getTestEnv = testCase "Prints empty metadata" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptySucceeds" $ do
    testDir <- getTestDir

    let trashDir = testDir </> ".trash"

    createDirectories [testDir, trashDir, trashDir </> "info", trashDir </> "files"]
    createFiles [trashDir </> "log"]

    metaArgList <- withSrArgsM ["metadata"]
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
