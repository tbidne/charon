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
metadata getTestEnv =
  testGoldenParamsOs
    $ MkGoldenParams
      { runner,
        testDesc = "Prints metadata",
        testName = testDirPrefix <> [osp|metadata|]
      }
  where
    runner = do
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
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- METADATA

        bs2 <- captureMetadataBs

        -- assert nothing changed
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        pure $ bs1 `concatBs` bs2

empty :: IO TestEnv -> TestTree
empty getTestEnv =
  testGoldenParamsOs
    $ MkGoldenParams
      { runner,
        testDesc = "Prints empty metadata",
        testName = testDirPrefix <> [osp|empty|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "emptySucceeds" $ do
        testDir <- getTestDir

        let trashDir = testDir </>! ".trash"

        createDirectories [testDir, trashDir, trashDir </>! "info", trashDir </>! "files"]
        createFiles [trashDir </>! "log"]

        bs <- captureMetadataBs
        pure bs

testDirPrefix :: OsString
testDirPrefix = [osstr|metadata_|]
