{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for r command.
module Functional.Commands.Restore
  ( tests,
  )
where

import Charon.Exception (RestoreCollisionE, TrashEntryNotFoundE)
import FileSystem.OsPath (unsafeDecode)
import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Restore Command"
    $ [ restoreOne testEnv',
        restoreMany testEnv',
        restoreIndices testEnv',
        restoreIndicesExit testEnv',
        restoreUnknownError testEnv',
        restoreCollisionError testEnv',
        restoreSimultaneousCollisionError testEnv',
        restoresSome testEnv',
        restoresWildcards testEnv',
        restoresSomeWildcards testEnv'
      ]
    <> wildcardLiteralTests testEnv'
  where
    testEnv' = appendTestDir "restore" <$> testEnv

restoreOne :: IO TestEnv -> TestTree
restoreOne getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores a single file",
        testName = testDirPrefix <> [osp|restoreOne|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreOne" $ do
        testDir <- getTestDir

        let trashDir = testDir </>! ".trash"
            f1 = testDir </>! "f1"

        delArgList <- withSrArgsPathsM ["delete", "-v"] [f1]

        -- SETUP

        createFiles [f1]
        assertPathsExist [f1]

        -- delete to trash first
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist [f1]

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE

        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "f1"]
        runCharon restoreArgList

        -- file assertions
        assertPathsExist [trashDir, f1]

        -- trash structure assertions
        assertFdoDirectorySizesM []
        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

restoreMany :: IO TestEnv -> TestTree
restoreMany getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores several paths",
        testName = testDirPrefix <> [osp|restoreMany|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreMany" $ do
        testDir <- getTestDir

        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete", "-v"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- SETUP
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks
          [ F fileLinkToDelete,
            D dirLinkToDelete,
            F $ testDir </>! "dir4" </>! "link"
          ]

        assertPathsExist ((testDir </>!) <$> ["dir1", "dir2/dir3"])
        assertPathsExist ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        assertSymlinksExist linksToDelete

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- RESTORE

        -- do not restore f2
        restoreArgList <-
          withSrArgsM
            [ "restore",
              "-v",
              "--no-prompt",
              "f1",
              "f3",
              "dir1",
              "dir2",
              "file-link"
            ]
        runCharon restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM ["dir4"]
        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

restoreIndices :: IO TestEnv -> TestTree
restoreIndices getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores with --indices",
        testName = testDirPrefix <> [osp|restoreIndices|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreIndices" $ do
        testDir <- getTestDir

        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete", "-v"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- SETUP
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]

        assertPathsExist ((testDir </>!) <$> ["dir1", "dir2/dir3"])
        assertPathsExist ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        assertSymlinksExist linksToDelete

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- RESTORE

        -- These are the indices for the same files as the previous test, in
        -- in sorted order.
        let modEnv = set' #strLine "2-3 5 7-8"

        -- do not restore f2
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "--indices"]
        runCharonEnv modEnv restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM ["dir4"]
        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

restoreIndicesExit :: IO TestEnv -> TestTree
restoreIndicesExit getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Exits --indices",
        testName = testDirPrefix <> [osp|restoreIndicesExit|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreIndicesExit" $ do
        testDir <- getTestDir

        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete", "-v"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- SETUP
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]

        assertPathsExist ((testDir </>!) <$> ["dir1", "dir2/dir3"])
        assertPathsExist ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        assertSymlinksExist linksToDelete

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- RESTORE

        -- Throwing a 'quit' in the middle. Should abort successfully.
        let modEnv = set' #strLine "2-3 quit 7-8"

        -- do not restore f2
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "--indices"]
        runCharonEnv modEnv restoreArgList

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

restoreUnknownError :: IO TestEnv -> TestTree
restoreUnknownError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restore unknown prints error",
        testName = testDirPrefix <> [osp|restoreUnknownError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreUnknownError" $ do
        testDir <- getTestDir

        let f1 = testDir </> [osp|f1|]
        delArgList <- withSrArgsPathsM ["delete", "-v"] [f1]

        -- SETUP

        -- technically we do not need to have anything in the trash to attempt
        -- a restore, but this way we can ensure the trash itself is set
        -- up (i.e. dir exists w/ index), so that we can test the restore
        -- failure only.
        clearDirectory testDir
        createFiles [f1]

        -- delete to trash first
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist [f1]

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "bad file"]
        exBs <- captureCharonTermBsE @TrashEntryNotFoundE testDir restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

restoreCollisionError :: IO TestEnv -> TestTree
restoreCollisionError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restore collision prints error",
        testName = testDirPrefix <> [osp|restoreCollisionError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreCollisionError" $ do
        testDir <- getTestDir

        let f1 = testDir </>! "f1"
        delArgList <- withSrArgsPathsM ["delete", "-v"] [f1]

        -- SETUP

        createFiles [f1]

        -- delete to trash first and recreate
        runCharon delArgList
        createFiles [f1]

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "f1"]
        exBs <- captureCharonTermBsE @RestoreCollisionE testDir restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

restoreSimultaneousCollisionError :: IO TestEnv -> TestTree
restoreSimultaneousCollisionError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restore simultaneous collision prints error",
        testName = testDirPrefix <> [osp|restoreSimultaneousCollisionError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoreSimultaneousCollisionError" $ do
        testDir <- getTestDir

        let f1 = testDir </>! "f1"
            f2 = testDir </>! "f2"
            f3 = testDir </>! "f3"
        delArgList <- withSrArgsPathsM ["delete", "-v"] [f1]

        -- SETUP

        createFiles [f1, f2, f3]

        -- delete twice
        runCharon (delArgList <> (unsafeDecode <$> [f2, f3]))
        createFiles [f1]
        runCharon delArgList

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "f1", "f1 (1)", "f2"]
        exBs <- captureCharonTermBsE @RestoreCollisionE testDir restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

restoresSome :: IO TestEnv -> TestTree
restoresSome getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores some, errors on others",
        testName = testDirPrefix <> [osp|restoresSome|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoresSome" $ do
        testDir <- getTestDir

        let realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
            filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
        delArgList <- withSrArgsPathsM ["delete", "-v"] realFiles

        -- setup
        clearDirectory testDir
        createFiles realFiles
        assertPathsExist realFiles

        -- delete to trash first
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist realFiles

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE
        restoreArgList <- withSrArgsM ("restore" : "-v" : "--no-prompt" : filesTryRestore)
        exBs <- captureCharonTermBsE @TrashEntryNotFoundE testDir restoreArgList

        -- file assertions
        assertPathsDoNotExist ((testDir </>!) <$> ["f3", "f4"])
        assertPathsExist ((testDir </>!) <$> ["f1", "f2"])

        -- trash structure assertions
        assertFdoDirectorySizesM []
        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

restoresWildcards :: IO TestEnv -> TestTree
restoresWildcards getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores several paths via wildcards",
        testName = testDirPrefix <> [osp|restoresWildcards|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoresWildcards" $ do
        testDir <- getTestDir

        let filesToRestore = (testDir </>!) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
            otherFiles = (testDir </>!) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]
        delArgList <- withSrArgsPathsM ["delete", "-v"] (filesToRestore <> otherFiles)

        -- SETUP
        createFiles (filesToRestore <> otherFiles)
        assertPathsExist (filesToRestore ++ otherFiles)

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToRestore ++ otherFiles)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE

        -- leave g alone
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "*f*"]
        runCharon restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []
        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

restoresSomeWildcards :: IO TestEnv -> TestTree
restoresSomeWildcards getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores some paths via wildcards",
        testName = testDirPrefix <> [osp|restoresSomeWildcards|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoresSomeWildcards" $ do
        testDir <- getTestDir

        let filesToRestore =
              [ "h1",
                "h2",
                "h3",
                "1h",
                "2h",
                "3h",
                "fooAbar"
              ]
            otherFiles =
              [ "fooBadbar",
                "fooXbar",
                "g1",
                "g2",
                "g3",
                "1g",
                "2g",
                "3g"
              ]
            testFilesToRestore = (testDir </>!) <$> filesToRestore
            testFiles = (testDir </>!) <$> filesToRestore <> otherFiles

        delArgList <- withSrArgsPathsM ["delete", "-v"] testFiles

        -- SETUP
        createFiles testFiles
        assertPathsExist testFiles

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist testFiles

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- RESTORE

        -- We want a collision to force an error
        createFiles [testDir </>! "fooBadbar"]

        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "*h*", "foo**bar", "*g*"]
        exBs <- captureCharonTermBsE @RestoreCollisionE testDir restoreArgList

        -- file assertions
        -- 1. Every restore attempt before fooBarBar should succeed.
        assertPathsExist testFilesToRestore

        -- trash structure assertions
        assertFdoDirectorySizesM []
        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

-- Wildcard literals are not valid in windows paths

#if !WINDOWS
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests testEnv =
  [ restoresLiteralWildcardOnly testEnv,
    restoresCombinedWildcardLiteral testEnv
  ]

restoresLiteralWildcardOnly :: IO TestEnv -> TestTree
restoresLiteralWildcardOnly getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores filename w/ literal wildcard",
        testName = testDirPrefix <> [osp|restoresLiteralWildcardOnly|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoresLiteralWildcardOnly" $ do
        testDir <- getTestDir

        let files = ["f1", "f2", "f3", "1f", "2f", "3f"]
            testFiles = (testDir </>!) <$> files
            testWcLiteral = testDir </>! "*"
        delArgList <- withSrArgsPathsM ["delete", "-v"] (testWcLiteral : testFiles)

        -- SETUP
        createFiles (testWcLiteral : testFiles)
        assertPathsExist (testWcLiteral : testFiles)

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (testWcLiteral : testFiles)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM [ ]

        -- RESTORE

        -- leave f alone
        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "\\*"]
        runCharon restoreArgList

        -- trash structure assertions
        assertFdoDirectorySizesM [ ]

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

restoresCombinedWildcardLiteral :: IO TestEnv -> TestTree
restoresCombinedWildcardLiteral getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Restores filename w/ literal * and wildcard",
        testName = testDirPrefix <> [osp|restoresCombinedWildcardLiteral|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "restoresCombinedWildcardLiteral" $ do
        testDir <- getTestDir

        let files = ["yxxfoo", "yxxbar", "yxxbaz"]
            wcLiterals = ["y*xxfoo", "y*xxbar", "y*xxbaz"]
            testFiles = (testDir </>!) <$> files
            testWcLiterals = (testDir </>!) <$> wcLiterals
        delArgList <- withSrArgsPathsM ["delete", "-v"] (testWcLiterals <> testFiles)

        -- SETUP
        createFiles (testWcLiterals <> testFiles)
        assertPathsExist (testWcLiterals <> testFiles)

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (testWcLiterals <> testFiles)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM [ ]

        -- RESTORE

        restoreArgList <- withSrArgsM ["restore", "-v", "--no-prompt", "y\\*xx*"]
        runCharon restoreArgList

        -- file assertions
        assertPathsExist testWcLiterals

        -- trash structure assertions
        assertFdoDirectorySizesM [ ]

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

#else
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests = const []
#endif

testDirPrefix :: OsString
testDirPrefix = [osstr|restore_|]
