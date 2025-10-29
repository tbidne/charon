{-# LANGUAGE QuasiQuotes #-}

-- | Tests for m command.
module Functional.Commands.Empty
  ( tests,
  )
where

import Charon.Backend.Default.Utils qualified as Default.Utils
import Functional.Prelude

-- NOTE: These tests currently rely on internal details for the trash
-- structure (see the usage of Default.Utils). If we ever get a non-compliant
-- backend, this will have to change.

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Empty Command"
    [ emptyTrash testEnv',
      emptyTrashTwice testEnv',
      emptyPrompt testEnv',
      missingInfoForcesDelete testEnv',
      missingPathsForcesDelete testEnv'
    ]
  where
    testEnv' = appendTestDir "empty" <$> testEnv

emptyTrash :: IO TestEnv -> TestTree
emptyTrash getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Empties trash",
        testName = testDirPrefix <> [osp|emptyTrash|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "emptyTrash" $ do
        testDir <- getTestDir

        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- setup
        -- test w/ a nested dir
        createDirectories (dirsToDelete <> [testDir </>! "dir2/dir3", testDir </>! "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
        assertPathsExist (filesToDelete ++ dirsToDelete)
        assertSymlinksExist linksToDelete

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- EMPTY

        emptyArgList <- withSrArgsM ["empty", "--prompt", "off"]
        runCharon emptyArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        bs2 <- captureIndexBs testDir
        pure $ bs1 <> bs2

emptyTrashTwice :: IO TestEnv -> TestTree
emptyTrashTwice getTestEnv = testCase "Calling empty twice does not error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptyTrashTwice" $ do
    emptyArgs <- withSrArgsM ["empty", "--prompt", "off"]
    runCharon emptyArgs
    runCharon emptyArgs

emptyPrompt :: IO TestEnv -> TestTree
emptyPrompt getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Empties w/ no response deletes nothing",
        testName = testDirPrefix <> [osp|emptyPrompt|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "emptyPrompt" $ do
        testDir <- getTestDir
        let fileDeleteNames = show @Int <$> [1 .. 5]
            fileDeletePaths = (testDir </>!) <$> fileDeleteNames

        delArgList <- withSrArgsPathsM ["delete"] fileDeletePaths

        -- setup
        -- test w/ a file in dir
        createFiles fileDeletePaths
        assertPathsExist fileDeletePaths

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist fileDeletePaths

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        -- EMPTY

        emptyArgList <- withSrArgsM ["empty"]
        runCharon emptyArgList

        -- trash structure assertions
        bs2 <- captureIndexBs testDir
        pure $ bs1 <> bs2

missingInfoForcesDelete :: IO TestEnv -> TestTree
missingInfoForcesDelete getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "empty --prompt off overwrites bad directory (no info.)",
        testName = testDirPrefix <> [osp|missingInfoForcesDelete|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "missingInfoForcesDelete" $ do
        testDir <- getTestDir

        let trashDir = testDir </> pathDotTrash
            filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]
        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

        -- setup
        -- test w/ a nested dir
        createDirectories (dirsToDelete <> [testDir </>! "dir2/dir3"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        assertPathsExist (filesToDelete ++ dirsToDelete)

        -- delete files
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        -- delete info dir, leaving trash dir in bad state
        clearDirectory (trashDir </> Default.Utils.pathInfo)

        emptyArgList <- withSrArgsM ["empty", "--prompt", "off"]
        runCharon emptyArgList

        assertPathsExist $ fmap (trashDir </>) [Default.Utils.pathInfo, Default.Utils.pathFiles]

        -- trash structure assertions
        bs2 <- captureIndexBs testDir
        pure $ bs1 <> bs2

missingPathsForcesDelete :: IO TestEnv -> TestTree
missingPathsForcesDelete getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "empty --prompt off overwrites bad directory (no paths/)",
        testName = testDirPrefix <> [osp|missingPathsForcesDelete|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "missingPathsForcesDelete" $ do
        testDir <- getTestDir

        let trashDir = testDir </> pathDotTrash
            filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

        -- setup
        -- test w/ a nested dir
        createDirectories (dirsToDelete <> [testDir </>! "dir2/dir3"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        assertPathsExist (filesToDelete ++ dirsToDelete)

        -- delete files
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        -- delete info dir, leaving trash dir in bad state
        clearDirectory (trashDir </> Default.Utils.pathFiles)

        emptyArgList <- withSrArgsM ["empty", "--prompt", "off"]
        runCharon emptyArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
        assertPathsExist $ fmap (trashDir </>) [Default.Utils.pathInfo, Default.Utils.pathFiles]

        -- trash structure assertions
        bs2 <- captureIndexBs testDir
        pure $ bs1 <> bs2

testDirPrefix :: OsString
testDirPrefix = [osstr|empty_|]
