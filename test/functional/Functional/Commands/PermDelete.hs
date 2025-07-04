{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- | Tests for x command.
module Functional.Commands.PermDelete
  ( tests,
  )
where

import Charon.Exception (TrashEntryNotFoundE)
import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Permanent Delete Command"
    $ [ deletesOne testEnv',
        deletesMany testEnv',
        deletesIndices testEnv',
        deletesIndicesExit testEnv',
        deleteUnknownError testEnv',
        deletesSome testEnv',
        deletesPrompt testEnv',
        deletesWildcards testEnv',
        deletesSomeWildcards testEnv',
        displaysAllData testEnv'
      ]
    <> wildcardLiteralTests testEnv'
  where
    testEnv' = appendTestDir "perm-delete" <$> testEnv

deletesOne :: IO TestEnv -> TestTree
deletesOne getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes a single file",
        testName = testDirPrefix <> [osp|deletesOne|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesOne" $ do
        testDir <- getTestDir
        let trashDir = testDir </> (testEnv ^. #trashDir)
            f1 = testDir </> [osstr|f1|]

        delArgList <- withSrArgsPathsM ["delete"] [f1]

        -- SETUP

        createFiles [f1]
        assertPathsExist [f1]

        -- delete to trash first
        liftIO $ runCharon delArgList

        -- file assertions
        assertPathsDoNotExist [f1]

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        -- PERMANENT DELETE

        permDelArgList <- withSrArgsM ["perm-delete", "f1", "--no-prompt"]
        liftIO $ runCharon permDelArgList

        -- file assertions
        assertPathsExist [trashDir]

        -- trash structure assertions
        assertFdoDirectorySizesM []
        bs2 <- captureIndexBs testDir

        pure $ bs1 `concatBs` bs2

deletesMany :: IO TestEnv -> TestTree
deletesMany getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes several paths",
        testName = testDirPrefix <> [osp|deletesMany|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesMany" $ do
        testDir <- getTestDir
        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- SETUP
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
        assertPathsExist (filesToDelete ++ dirsToDelete)
        assertSymlinksExist linksToDelete

        liftIO $ runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- PERMANENT DELETE

        -- leave f2 alone
        permDelArgList <-
          withSrArgsM
            [ "perm-delete",
              "f1",
              "f3",
              "dir1",
              "dir2",
              "file-link",
              "--no-prompt"
            ]
        liftIO $ runCharon permDelArgList

        -- trash structure assertions
        bs2 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir4"]

        pure $ bs1 `concatBs` bs2

deletesIndices :: IO TestEnv -> TestTree
deletesIndices getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes with --indices",
        testName = testDirPrefix <> [osp|deletesIndices|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesIndices" $ do
        testDir <- getTestDir
        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- SETUP
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
        assertPathsExist (filesToDelete ++ dirsToDelete)
        assertSymlinksExist linksToDelete

        liftIO $ runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- PERMANENT DELETE

        -- These are the indices for the same files as the previous test, in
        -- in sorted order.
        let modEnv = set' #strLine "2-3 5 7-8"

        -- leave f2 alone
        --
        -- NOTE: It would be nice to capture the output here, to assert the
        -- terminal contents since it's non-trivial for --indices delete.
        -- Alas, the output is non-deterministic since it's a formatted
        -- table, where the header line is dependent on the longest path
        -- __before__ we strip the test directory.
        permDelArgList <- withSrArgsM ["perm-delete", "--indices", "--no-prompt"]
        liftIO $ runCharonEnv modEnv permDelArgList

        -- trash structure assertions
        bs2 <- captureIndexBs testDir
        assertFdoDirectorySizesM ["dir4"]
        pure $ bs1 `concatBs` bs2

deletesIndicesExit :: IO TestEnv -> TestTree
deletesIndicesExit getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Exits --indices",
        testName = testDirPrefix <> [osp|deletesIndicesExit|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesIndicesExit" $ do
        testDir <- getTestDir
        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- SETUP
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
        assertPathsExist (filesToDelete ++ dirsToDelete)
        assertSymlinksExist linksToDelete

        liftIO $ runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        -- PERMANENT DELETE

        -- Throwing an 'exit' in the middle. Should abort successfully.
        let modEnv = set' #strLine "2-3 exit 7-8"

        -- leave f2 alone
        permDelArgList <- withSrArgsM ["perm-delete", "--indices", "--no-prompt"]
        liftIO $ runCharonEnv modEnv permDelArgList

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

deleteUnknownError :: IO TestEnv -> TestTree
deleteUnknownError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Delete unknown prints error",
        testName = testDirPrefix <> [osp|deleteUnknownError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deleteUnknownError" $ do
        testDir <- getTestDir

        let f1 = testDir </>! "f1"

        delArgList <- withSrArgsPathsM ["delete"] [f1]

        -- SETUP

        -- technically we do not need to have anything in the trash to attempt
        -- a permanent delete, but this way we can ensure the trash itself is set
        -- up (i.e. dir exists w/ index), so that we can test the perm charon
        -- failure only.
        createFiles [f1]
        assertPathsExist [f1]

        -- delete to trash first
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist [f1]

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        -- PERMANENT DELETE
        permDelArgList <- withSrArgsM ["perm-delete", "bad file", "--no-prompt"]
        exBs <- captureCharonTermBsE @TrashEntryNotFoundE testDir permDelArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

deletesSome :: IO TestEnv -> TestTree
deletesSome getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes some, errors on others",
        testName = testDirPrefix <> [osp|deletesSome|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesSome" $ do
        testDir <- getTestDir

        let realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
            filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]

        delArgList <- withSrArgsPathsM ["delete"] realFiles

        -- setup
        createFiles realFiles
        assertPathsExist realFiles

        -- delete to trash first
        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist realFiles

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- PERMANENT DELETE
        permDelArgList <-
          withSrArgsM
            ("perm-delete" : filesTryPermDelete ++ ["--no-prompt"])
        exBs <- captureCharonTermBsE @TrashEntryNotFoundE testDir permDelArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

deletesPrompt :: IO TestEnv -> TestTree
deletesPrompt getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes several paths with prompt",
        testName = testDirPrefix <> [osp|deletesPrompt|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesPrompt" $ do
        testDir <- getTestDir
        let fileDeleteNames = show @Int <$> [1 .. 5]
            fileDeletePaths = (testDir </>!) <$> fileDeleteNames

        delArgList <- withSrArgsPathsM ["delete"] fileDeletePaths

        -- SETUP
        createFiles fileDeletePaths
        assertPathsExist fileDeletePaths

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist fileDeletePaths

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- PERMANENT DELETE

        permDelArgList <- withSrArgsM ("perm-delete" : fileDeleteNames)
        runCharon permDelArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

deletesWildcards :: IO TestEnv -> TestTree
deletesWildcards getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes several paths via wildcards",
        testName = testDirPrefix <> [osp|deletesWildcards|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesWildcards" $ do
        testDir <- getTestDir
        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
            otherFiles = (testDir </>!) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> otherFiles)

        -- SETUP
        createFiles (filesToDelete <> otherFiles)
        assertPathsExist (filesToDelete ++ otherFiles)

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ otherFiles)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- PERMANENT DELETE

        -- leave g alone
        permDelArgList <- withSrArgsM ["perm-delete", "*f*", "--no-prompt"]
        runCharon permDelArgList

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

deletesSomeWildcards :: IO TestEnv -> TestTree
deletesSomeWildcards getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Deletes some paths via wildcards",
        testName = testDirPrefix <> [osp|deletesSomeWildcards|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesSomeWildcards" $ do
        testDir <- getTestDir
        -- Want something that comes alphabetically before fooBadBar (fooAbar), to
        -- test partial delete.
        let files =
              [ "h1",
                "h2",
                "h3",
                "1h",
                "2h",
                "3h",
                "fooAbar",
                "fooBadbar",
                "fooXbar",
                "g1",
                "g2",
                "g3",
                "1g",
                "2g",
                "3g"
              ]
            testFiles = (testDir </>!) <$> files

        delArgList <- withSrArgsPathsM ["delete"] testFiles

        -- SETUP
        createFiles testFiles
        assertPathsExist testFiles

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist testFiles

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- PERMANENT DELETE

        -- NOTE: fooBadbar has been mocked in Prelude such that an attempted
        -- delete will fail. This is how this test works.
        permDelArgList <- withSrArgsM ["perm-delete", "*h*", "foo**bar", "*g*", "--no-prompt"]
        exBs <- captureCharonTermBsE @TextException testDir permDelArgList

        -- file assertions
        -- 1. Everything still gone from original location
        assertPathsDoNotExist testFiles

        -- trash structure assertions
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` exBs `concatBs` bs2

-- Wildcard literals are not valid in windows paths

#if !WINDOWS
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests testEnv =
  [ deletesLiteralWildcardOnly testEnv,
    deletesCombinedWildcardLiteral testEnv
  ]

deletesLiteralWildcardOnly :: IO TestEnv -> TestTree
deletesLiteralWildcardOnly getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes filename w/ literal wildcard",
        testName = testDirPrefix <> [osp|deletesLiteralWildcardOnly|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesLiteralWildcardOnly" $ do
        testDir <- getTestDir
        let files = ["f1", "f2", "f3", "1f", "2f", "3f"]
            testFiles = (testDir </>!) <$> files
            testWcLiteral = testDir </>! "*"
        
        delArgList <- withSrArgsPathsM ["delete"] (testWcLiteral : testFiles)

        -- SETUP
        clearDirectory testDir
        createFiles (testWcLiteral : testFiles)
        assertPathsExist (testWcLiteral : testFiles)

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (testWcLiteral : testFiles)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM []

        -- PERMANENT DELETE

        -- leave f alone
        permDelArgList <- withSrArgsM ["perm-delete", "\\*", "--no-prompt"]
        runCharon permDelArgList

        -- trash structure assertions
        assertFdoDirectorySizesM [ ]

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

deletesCombinedWildcardLiteral :: IO TestEnv -> TestTree
deletesCombinedWildcardLiteral getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Permanently deletes filename w/ literal * and wildcard",
        testName = testDirPrefix <> [osp|deletesCombinedWildcardLiteral|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "deletesCombinedWildcardLiteral" $ do
        testDir <- getTestDir
        let files = ["xxfoo", "xxbar", "xxbaz"]
            wcLiterals = ["y*xxfoo", "y*xxbar", "y*xxbaz"]
            testFiles = (testDir </>!) <$> files
            testWcLiterals = (testDir </>!) <$> wcLiterals
        
        delArgList <- withSrArgsPathsM ["delete"] (testWcLiterals <> testFiles)

        -- SETUP
        createFiles (testWcLiterals <> testFiles)
        assertPathsExist (testWcLiterals <> testFiles)

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (testWcLiterals <> testFiles)

        -- trash structure assertions
        bs1 <- captureIndexBs testDir
        assertFdoDirectorySizesM [ ]

        -- PERMANENT DELETE

        -- leave f alone
        permDelArgList <- withSrArgsM ["perm-delete", "y\\*xx*", "--no-prompt"]
        runCharon permDelArgList

        -- trash structure assertions
        assertFdoDirectorySizesM [ ]

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` bs2

#else
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests = const []
#endif

displaysAllData :: IO TestEnv -> TestTree
displaysAllData getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Displays all data for each backend",
        testName = testDirPrefix <> [osp|displaysAllData|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "displaysAllData" $ do
        testDir <- getTestDir
        let f1 = testDir </>! "f1"

        delArgList <- withSrArgsPathsM ["delete"] [f1]

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

        -- PERMANENT DELETE

        -- NOTE: we don't actually delete f1 as the first mocked answer is 'n'
        -- (see altAnswers in FuncEnv), but it doesn't matter as this test is only
        -- concerned with grabbing the terminal data for an unforced delete.
        permDelArgList <- withSrArgsM ["perm-delete", "f1"]
        (terminalResult, _) <- captureCharonLogs permDelArgList
        let termBs = terminalToBs testDir terminalResult

        -- assert terminal displays all data for f1
        assertFdoDirectorySizesM []

        bs2 <- captureIndexBs testDir
        pure $ bs1 `concatBs` termBs `concatBs` bs2

testDirPrefix :: OsString
testDirPrefix = [osstr|perm_delete_|]
