{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for m command.
module Functional.Commands.Metadata
  ( tests,
  )
where

import Functional.Prelude

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

    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
        fileLinkToDelete = testDir </> [osp|file-link|]
        dirLinkToDelete = testDir </> [osp|dir-link|]
        linksToDelete = [fileLinkToDelete, dirLinkToDelete]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

    -- setup
    clearDirectory testDir
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
    assertPathsExist (filesToDelete ++ dirsToDelete)

    runCharon delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
    assertSymlinksDoNotExist linksToDelete

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15),
          ("dir4", PathTypeDirectory, 10),
          ("dir-link", PathTypeSymbolicLink, 5),
          ("file-link", PathTypeSymbolicLink, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

    -- METADATA

    metaArgList <- withSrArgsM ["metadata"]
    (metadataResult, _) <- captureCharonLogs metaArgList

    -- assert nothing changed
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    assertMatches expectedMetadata metadataResult

    -- trash structure assertions
    (metadataIdxSet, metadatMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet metadataIdxSet
    delExpectedMetadata @=? metadatMetadata
    assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]
  where
    delExpectedMetadata = mkMetadata 8 7 0 55

{- ORMOLU_DISABLE -}

    expectedMetadata =
      Exact
        <$> [ "Entries:      8",
              "Total Files:  7",
              "Log size:     0.00B",
#if WINDOWS
              "Size:         0.00B",
#else
              "Size:         55.00B",
#endif
              ""
            ]
{- ORMOLU_ENABLE -}

empty :: IO TestEnv -> TestTree
empty getTestEnv = testCase "Prints empty metadata" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptySucceeds" $ do
    testDir <- getTestDir

    let trashDir = testDir </>! ".trash"

    createDirectories [testDir, trashDir, trashDir </>! "info", trashDir </>! "files"]
    createFiles [trashDir </>! "log"]

    metaArgList <- withSrArgsM ["metadata"]
    (result, _) <- captureCharonLogs metaArgList

    assertMatches expectedTerminal result
  where
    expectedTerminal =
      Exact
        <$> [ "Entries:      0",
              "Total Files:  0",
              "Log size:     0.00B",
              "Size:         0.00B",
              ""
            ]
