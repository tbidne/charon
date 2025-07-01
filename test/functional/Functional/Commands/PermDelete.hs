{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- | Tests for x command.
module Functional.Commands.PermDelete
  ( tests,
  )
where

import Charon.Data.Metadata qualified as Metadata
import Charon.Exception (TrashEntryNotFoundE)
import Data.HashSet qualified as HashSet
import Data.Text qualified as T
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
deletesOne getTestEnv = testCase "Permanently deletes a single file" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesOne" $ do
    testDir <- getTestDir
    let trashDir = testDir </> (testEnv ^. #trashDir)
        f1 = testDir </>! "f1"

    delArgList <- withSrArgsPathsM ["delete"] [f1]

    -- SETUP

    createFiles [f1]
    assertPathsExist [f1]

    -- delete to trash first
    liftIO $ runCharon delArgList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM

    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    assertSetEq delExpectedIdxSet delIdxSet
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    permDelArgList <- withSrArgsM ["perm-delete", "f1", "--no-prompt"]
    liftIO $ runCharon permDelArgList

    -- file assertions
    assertPathsExist [trashDir]

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 1 1 0 5

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = Metadata.empty

deletesMany :: IO TestEnv -> TestTree
deletesMany getTestEnv = testCase "Permanently deletes several paths" $ do
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

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

    -- PERMANENT DELETE

    -- leave f2 alone
    permDelArgList <- withSrArgsM ["perm-delete", "f1", "f3", "dir1", "dir2", "file-link", "--no-prompt"]
    liftIO $ runCharon permDelArgList

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("f2", PathTypeFile, 5),
          ("dir4", PathTypeDirectory, 10),
          ("dir-link", PathTypeSymbolicLink, 5)
        ]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM ["dir4"]
  where
    delExpectedMetadata = mkMetadata 8 7 0 55

    permDelExpectedMetadata = mkMetadata 3 3 0 20

deletesIndices :: IO TestEnv -> TestTree
deletesIndices getTestEnv = testCase "Permanently deletes with --indices" $ do
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

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

    -- PERMANENT DELETE

    -- These are the indices for the same files as the previous test, in
    -- in sorted order.
    let modEnv = set' #strLine "2-3 5 7-8"

    -- leave f2 alone
    permDelArgList <- withSrArgsM ["perm-delete", "--indices", "--no-prompt"]
    liftIO $ runCharonEnv modEnv permDelArgList

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("f2", PathTypeFile, 5),
          ("dir4", PathTypeDirectory, 10),
          ("dir-link", PathTypeSymbolicLink, 5)
        ]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM ["dir4"]
  where
    delExpectedMetadata = mkMetadata 8 7 0 55

    permDelExpectedMetadata = mkMetadata 3 3 0 20

deletesIndicesExit :: IO TestEnv -> TestTree
deletesIndicesExit getTestEnv = testCase "Exits --indices" $ do
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

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

    -- PERMANENT DELETE

    -- Throwing an 'exit' in the middle. Should abort successfully.
    let modEnv = set' #strLine "2-3 exit 7-8"

    -- leave f2 alone
    permDelArgList <- withSrArgsM ["perm-delete", "--indices", "--no-prompt"]
    liftIO $ runCharonEnv modEnv permDelArgList

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet permDelIdxSet
    delExpectedMetadata @=? permDelMetadata
  where
    delExpectedMetadata = mkMetadata 8 7 0 55

deleteUnknownError :: IO TestEnv -> TestTree
deleteUnknownError getTestEnv = testCase "Delete unknown prints error" $ do
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
    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE
    permDelArgList <- withSrArgsM ["perm-delete", "bad file", "--no-prompt"]
    (ex, term) <- liftIO $ captureCharonExceptionTerminal @TrashEntryNotFoundE permDelArgList

    -- assert exception
    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet permDelIdxSet
    delExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Exact "No entry for 'bad file'"
    expectedTerm = []

    delExpectedMetadata = mkMetadata 1 1 0 5

deletesSome :: IO TestEnv -> TestTree
deletesSome getTestEnv = testCase "Deletes some, errors on others" $ do
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
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f5", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- PERMANENT DELETE
    permDelArgList <-
      withSrArgsM
        ("perm-delete" : filesTryPermDelete ++ ["--no-prompt"])
    (ex, term) <- captureCharonExceptionTerminal @TrashEntryNotFoundE permDelArgList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    permDelExpectedIdxSet <- mkPathDataSetM [("f5", PathTypeFile, 5)]
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Exact "No entry for 'f3'"
    expectedTerm =
      [ Exact "Permanently deleted paths:",
        Exact "- f1",
        Exact "- f2",
        Exact ""
      ]

    delExpectedMetadata = mkMetadata 3 3 0 15

    permDelExpectedMetadata = mkMetadata 1 1 0 5

deletesPrompt :: IO TestEnv -> TestTree
deletesPrompt getTestEnv = testCase "Permanently deletes several paths with prompt" $ do
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
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("1", PathTypeFile, 5),
          ("2", PathTypeFile, 5),
          ("3", PathTypeFile, 5),
          ("4", PathTypeFile, 5),
          ("5", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- PERMANENT DELETE

    permDelArgList <- withSrArgsM ("perm-delete" : fileDeleteNames)
    runCharon permDelArgList

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("1", PathTypeFile, 5),
          ("3", PathTypeFile, 5),
          ("5", PathTypeFile, 5)
        ]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 5 5 0 25

    permDelExpectedMetadata = mkMetadata 3 3 0 15

deletesWildcards :: IO TestEnv -> TestTree
deletesWildcards getTestEnv = testCase "Permanently deletes several paths via wildcards" $ do
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
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("1f", PathTypeFile, 5),
          ("2f", PathTypeFile, 5),
          ("3f", PathTypeFile, 5),
          ("g1", PathTypeFile, 5),
          ("g2", PathTypeFile, 5),
          ("g3", PathTypeFile, 5),
          ("1g", PathTypeFile, 5),
          ("2g", PathTypeFile, 5),
          ("3g", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- PERMANENT DELETE

    -- leave g alone
    permDelArgList <- withSrArgsM ["perm-delete", "*f*", "--no-prompt"]
    runCharon permDelArgList

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("g1", PathTypeFile, 5),
          ("g2", PathTypeFile, 5),
          ("g3", PathTypeFile, 5),
          ("1g", PathTypeFile, 5),
          ("2g", PathTypeFile, 5),
          ("3g", PathTypeFile, 5)
        ]
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 12 12 0 60

    permDelExpectedMetadata = mkMetadata 6 6 0 30

deletesSomeWildcards :: IO TestEnv -> TestTree
deletesSomeWildcards getTestEnv = testCase "Deletes some paths via wildcards" $ do
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
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("h1", PathTypeFile, 5),
          ("h2", PathTypeFile, 5),
          ("h3", PathTypeFile, 5),
          ("1h", PathTypeFile, 5),
          ("2h", PathTypeFile, 5),
          ("3h", PathTypeFile, 5),
          ("fooAbar", PathTypeFile, 5),
          ("fooBadbar", PathTypeFile, 5),
          ("fooXbar", PathTypeFile, 5),
          ("g1", PathTypeFile, 5),
          ("g2", PathTypeFile, 5),
          ("g3", PathTypeFile, 5),
          ("1g", PathTypeFile, 5),
          ("2g", PathTypeFile, 5),
          ("3g", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- PERMANENT DELETE

    -- NOTE: fooBadbar has been mocked in Prelude such that an attempted
    -- delete will fail. This is how this test works.
    permDelArgList <- withSrArgsM ["perm-delete", "*h*", "foo**bar", "*g*", "--no-prompt"]
    runCharonException @SomeException permDelArgList

    -- file assertions
    -- 1. Everything still gone from original location
    assertPathsDoNotExist testFiles

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("fooBadbar", PathTypeFile, 5),
          ("fooXbar", PathTypeFile, 5),
          ("g1", PathTypeFile, 5),
          ("g2", PathTypeFile, 5),
          ("g3", PathTypeFile, 5),
          ("1g", PathTypeFile, 5),
          ("2g", PathTypeFile, 5),
          ("3g", PathTypeFile, 5)
        ]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 15 15 0 75
    permDelExpectedMetadata = mkMetadata 8 8 0 40

-- Wildcard literals are not valid in windows paths

#if !WINDOWS
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests testEnv =
  [ deletesLiteralWildcardOnly testEnv,
    deletesCombinedWildcardLiteral testEnv
  ]

deletesLiteralWildcardOnly :: IO TestEnv -> TestTree
deletesLiteralWildcardOnly getTestEnv = testCase "Permanently deletes filename w/ literal wildcard" $ do
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
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("1f", PathTypeFile, 5),
          ("2f", PathTypeFile, 5),
          ("3f", PathTypeFile, 5),
          ("*", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM [ ]

    -- PERMANENT DELETE

    -- leave f alone
    permDelArgList <- withSrArgsM ["perm-delete", "\\*", "--no-prompt"]
    runCharon permDelArgList

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("1f", PathTypeFile, 5),
          ("2f", PathTypeFile, 5),
          ("3f", PathTypeFile, 5)
        ]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM [ ]
  where
    delExpectedMetadata = mkMetadata 7 7 0 35
    permDelExpectedMetadata = mkMetadata 6 6 0 30

deletesCombinedWildcardLiteral :: IO TestEnv -> TestTree
deletesCombinedWildcardLiteral getTestEnv = testCase desc $ do
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
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("xxfoo", PathTypeFile, 5),
          ("xxbar", PathTypeFile, 5),
          ("xxbaz", PathTypeFile, 5),
          ("y*xxfoo", PathTypeFile, 5),
          ("y*xxbar", PathTypeFile, 5),
          ("y*xxbaz", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM [ ]

    -- PERMANENT DELETE

    -- leave f alone
    permDelArgList <- withSrArgsM ["perm-delete", "y\\*xx*", "--no-prompt"]
    runCharon permDelArgList

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("xxfoo", PathTypeFile, 5),
          ("xxbar", PathTypeFile, 5),
          ("xxbaz", PathTypeFile, 5)
        ]
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
    assertFdoDirectorySizesM [ ]
  where
    desc = "Permanently deletes filename w/ literal * and wildcard"

    delExpectedMetadata = mkMetadata 6 6 0 30
    permDelExpectedMetadata = mkMetadata 3 3 0 15

#else
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests = const []
#endif

{- ORMOLU_DISABLE -}

displaysAllData :: IO TestEnv -> TestTree
displaysAllData getTestEnv = testCase "Displays all data for each backend" $ do
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
    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM [ ]

    -- PERMANENT DELETE

    -- NOTE: we don't actually delete f1 as the first mocked answer is 'n'
    -- (see altAnswers in FuncEnv), but it doesn't matter as this test is only
    -- concerned with grabbing the terminal data for an unforced delete.
    permDelArgList <- withSrArgsM ["perm-delete", "f1"]
    (terminalResult, _) <- captureCharonLogs permDelArgList

    -- assert terminal displays all data for f1
    assertMatches expectedTerminal terminalResult
    assertFdoDirectorySizesM [ ]
  where
    expectedTerminal =
      [ Exact "Name:      f1",
        Outfixes
          "Original:"
          [combineFps ["displaysAllData"]]
          "/f1",
        Exact "Type:      File",
        Exact "Size:      0.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        -- Leaving off the "(y/n)?" suffix as the windows tests replaces all
        -- backslashes with forward slashes.
        Prefix "Permanently delete"
      ]

    delExpectedMetadata = mkMetadata 1 1 0 5

{- ORMOLU_ENABLE -}

combineFps :: [FilePath] -> Text
combineFps =
  T.pack
    . foldFilePathsAcc "perm-delete"
