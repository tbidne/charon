{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Tests for r command.
module Functional.Commands.Restore
  ( tests,
  )
where

import Charon.Data.Metadata qualified as Metadata
import Charon.Exception (RestoreCollisionE, TrashEntryNotFoundE)
import Data.HashSet qualified as HashSet
import FileSystem.OsPath (unsafeDecode)
import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Restore Command"
    $ [ restoreOne testEnv',
        restoreMany testEnv',
        restoreIndices testEnv',
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
restoreOne getTestEnv = testCase "Restores a single file" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreOne" $ do
    testDir <- getTestDir

    let trashDir = testDir </>! ".trash"
        f1 = testDir </>! "f1"

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
    assertFdoDirectorySizesM []

    -- RESTORE

    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "f1"]
    runCharon restoreArgList

    -- file assertions
    assertPathsExist [trashDir, f1]

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 1 1 0 5

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = Metadata.empty

restoreMany :: IO TestEnv -> TestTree
restoreMany getTestEnv = testCase "Restores several paths" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreMany" $ do
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

    assertPathsExist ((testDir </>!) <$> ["dir1", "dir2/dir3"])
    assertPathsExist ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    assertSymlinksExist linksToDelete

    runCharon delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

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

    -- RESTORE

    -- do not restore f2
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "f1", "f3", "dir1", "dir2", "file-link"]
    runCharon restoreArgList

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM
        [ ("f2", PathTypeFile, 5),
          ("dir4", PathTypeDirectory, 10),
          ("dir-link", PathTypeSymbolicLink, 5)
        ]

    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM ["dir4"]
  where
    delExpectedMetadata = mkMetadata 8 7 0 55
    restoreExpectedMetadata = mkMetadata 3 3 0 20

restoreIndices :: IO TestEnv -> TestTree
restoreIndices getTestEnv = testCase "Restores with --indices" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreIndices" $ do
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

    assertPathsExist ((testDir </>!) <$> ["dir1", "dir2/dir3"])
    assertPathsExist ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    assertSymlinksExist linksToDelete

    runCharon delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete ++ linksToDelete)

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

    -- RESTORE

    -- These are the indices for the same files as the previous test, in
    -- in sorted order.
    let modEnv = set' #strLine "2-3 5 7-8"

    -- do not restore f2
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "--indices"]
    runCharonEnv modEnv restoreArgList

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM
        [ ("f2", PathTypeFile, 5),
          ("dir4", PathTypeDirectory, 10),
          ("dir-link", PathTypeSymbolicLink, 5)
        ]

    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM ["dir4"]
  where
    delExpectedMetadata = mkMetadata 8 7 0 55
    restoreExpectedMetadata = mkMetadata 3 3 0 20

restoreUnknownError :: IO TestEnv -> TestTree
restoreUnknownError getTestEnv = testCase "Restore unknown prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreUnknownError" $ do
    testDir <- getTestDir

    let f1 = testDir </> [osp|f1|]
    delArgList <- withSrArgsPathsM ["delete"] [f1]

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
    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- RESTORE
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "bad file"]
    (ex, term) <- captureCharonExceptionTerminal @TrashEntryNotFoundE restoreArgList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet restoreIdxSet
    delExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Exact "No entry for 'bad file'"
    expectedTerm = []

    delExpectedMetadata = mkMetadata 1 1 0 5

restoreCollisionError :: IO TestEnv -> TestTree
restoreCollisionError getTestEnv = testCase "Restore collision prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreCollisionError" $ do
    testDir <- getTestDir

    let f1 = testDir </>! "f1"
    delArgList <- withSrArgsPathsM ["delete"] [f1]

    -- SETUP

    createFiles [f1]

    -- delete to trash first and recreate
    runCharon delArgList
    createFiles [f1]

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- RESTORE
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "f1"]
    (ex, term) <- captureCharonExceptionTerminal @RestoreCollisionE restoreArgList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet restoreIdxSet
    delExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    expectedEx =
      Outfix
        "Cannot restore the trash file 'f1' as one exists at the original location"
        "restore/restoreCollisionError/f1'"
    expectedTerm = []

    delExpectedMetadata = mkMetadata 1 1 0 5

restoreSimultaneousCollisionError :: IO TestEnv -> TestTree
restoreSimultaneousCollisionError getTestEnv = testCase desc $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreSimultaneousCollisionError" $ do
    testDir <- getTestDir

    let f1 = testDir </>! "f1"
        f2 = testDir </>! "f2"
        f3 = testDir </>! "f3"
    delArgList <- withSrArgsPathsM ["delete"] [f1]

    -- SETUP

    createFiles [f1, f2, f3]

    -- delete twice
    runCharon (delArgList <> (unsafeDecode <$> [f2, f3]))
    createFiles [f1]
    runCharon delArgList

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM2
        [ ("f1", "f1", PathTypeFile, 5),
          ("f1 (1)", "f1", PathTypeFile, 5),
          ("f2", "f2", PathTypeFile, 5),
          ("f3", "f3", PathTypeFile, 5)
        ]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM []

    -- RESTORE
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "f1", "f1 (1)", "f2"]
    (ex, term) <- captureCharonExceptionTerminal @RestoreCollisionE restoreArgList

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM2
        [ ("f1 (1)", "f1", PathTypeFile, 5),
          ("f2", "f2", PathTypeFile, 5),
          ("f3", "f3", PathTypeFile, 5)
        ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    desc = "Restore simultaneous collision prints error"
    expectedEx =
      Outfix
        "Cannot restore the trash file 'f1 (1)' as one exists at the original location"
        "estore/restoreSimultaneousCollisionError/f1'"
    expectedTerm =
      [ Exact "Restored paths:",
        Outfix "- " "restore/restoreSimultaneousCollisionError/f1",
        Exact ""
      ]

    delExpectedMetadata = mkMetadata 4 4 0 20
    restoreExpectedMetadata = mkMetadata 3 3 0 15

restoresSome :: IO TestEnv -> TestTree
restoresSome getTestEnv = testCase "Restores some, errors on others" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoresSome" $ do
    testDir <- getTestDir

    let realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
        filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
    delArgList <- withSrArgsPathsM ["delete"] realFiles

    -- setup
    clearDirectory testDir
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

    -- RESTORE
    restoreArgList <- withSrArgsM ("restore" : "--no-prompt" : filesTryRestore)
    (ex, term) <- captureCharonExceptionTerminal @TrashEntryNotFoundE restoreArgList

    -- file assertions
    assertPathsDoNotExist ((testDir </>!) <$> ["f3", "f4"])
    assertPathsExist ((testDir </>!) <$> ["f1", "f2"])

    assertMatch expectedEx ex
    assertMatches expectedTerm term

    -- trash structure assertions
    restoreExpectedIdxSet <- mkPathDataSetM [("f5", PathTypeFile, 5)]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    expectedEx = Exact "No entry for 'f3'"
    expectedTerm =
      [ Exact "Restored paths:",
        Outfixes "- " [] "restore/restoresSome/f1",
        Outfixes "- " [] "restore/restoresSome/f2",
        Exact ""
      ]

    delExpectedMetadata = mkMetadata 3 3 0 15

    restoreExpectedMetadata = mkMetadata 1 1 0 5

restoresWildcards :: IO TestEnv -> TestTree
restoresWildcards getTestEnv = testCase "Restores several paths via wildcards" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoresWildcards" $ do
    testDir <- getTestDir

    let filesToRestore = (testDir </>!) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
        otherFiles = (testDir </>!) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]
    delArgList <- withSrArgsPathsM ["delete"] (filesToRestore <> otherFiles)

    -- SETUP
    createFiles (filesToRestore <> otherFiles)
    assertPathsExist (filesToRestore ++ otherFiles)

    runCharon delArgList

    -- file assertions
    assertPathsDoNotExist (filesToRestore ++ otherFiles)

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

    -- RESTORE

    -- leave g alone
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "*f*"]
    runCharon restoreArgList

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM
        [ ("g1", PathTypeFile, 5),
          ("g2", PathTypeFile, 5),
          ("g3", PathTypeFile, 5),
          ("1g", PathTypeFile, 5),
          ("2g", PathTypeFile, 5),
          ("3g", PathTypeFile, 5)
        ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 12 12 0 60
    restoreExpectedMetadata = mkMetadata 6 6 0 30

restoresSomeWildcards :: IO TestEnv -> TestTree
restoresSomeWildcards getTestEnv = testCase "Restores some paths via wildcards" $ do
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

    -- RESTORE

    -- We want a collision to force an error
    createFiles [testDir </>! "fooBadbar"]

    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "*h*", "foo**bar", "*g*"]
    runCharonException @RestoreCollisionE restoreArgList

    -- file assertions
    -- 1. Every restore attempt before fooBarBar should succeed.
    assertPathsExist testFilesToRestore

    -- trash structure assertions
    restoreExpectedIdxSet <-
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
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 15 15 0 75
    restoreExpectedMetadata = mkMetadata 8 8 0 40

-- Wildcard literals are not valid in windows paths

#if !WINDOWS
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests testEnv =
  [ restoresLiteralWildcardOnly testEnv,
    restoresCombinedWildcardLiteral testEnv
  ]

restoresLiteralWildcardOnly :: IO TestEnv -> TestTree
restoresLiteralWildcardOnly getTestEnv = testCase "Restores filename w/ literal wildcard" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoresLiteralWildcardOnly" $ do
    testDir <- getTestDir

    let files = ["f1", "f2", "f3", "1f", "2f", "3f"]
        testFiles = (testDir </>!) <$> files
        testWcLiteral = testDir </>! "*"
    delArgList <- withSrArgsPathsM ["delete"] (testWcLiteral : testFiles)

    -- SETUP
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

    -- RESTORE

    -- leave f alone
    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "\\*"]
    runCharon restoreArgList

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("1f", PathTypeFile, 5),
          ("2f", PathTypeFile, 5),
          ("3f", PathTypeFile, 5)
        ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM [ ]
  where
    delExpectedMetadata = mkMetadata 7 7 0 35
    restoreExpectedMetadata = mkMetadata 6 6 0 30

restoresCombinedWildcardLiteral :: IO TestEnv -> TestTree
restoresCombinedWildcardLiteral getTestEnv = testCase desc $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoresCombinedWildcardLiteral" $ do
    testDir <- getTestDir

    let files = ["yxxfoo", "yxxbar", "yxxbaz"]
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
    delExpectedIdxSet <- mkPathDataSetM
      [ ("yxxfoo", PathTypeFile, 5),
        ("yxxbar", PathTypeFile, 5),
        ("yxxbaz", PathTypeFile, 5),
        ("y*xxfoo", PathTypeFile, 5),
        ("y*xxbar", PathTypeFile, 5),
        ("y*xxbaz", PathTypeFile, 5)
      ]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata
    assertFdoDirectorySizesM [ ]

    -- RESTORE

    restoreArgList <- withSrArgsM ["restore", "--no-prompt", "y\\*xx*"]
    runCharon restoreArgList

    -- file assertions
    assertPathsExist testWcLiterals

    -- trash structure assertions
    restoreExpectedIdxSet <- mkPathDataSetM
      [ ("yxxfoo", PathTypeFile, 5),
        ("yxxbar", PathTypeFile, 5),
        ("yxxbaz", PathTypeFile, 5)
      ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreArgListExpectedMetadata @=? restoreMetadata
    assertFdoDirectorySizesM [ ]
  where
    desc = "Restores filename w/ literal * and wildcard"

    delExpectedMetadata = mkMetadata 6 6 0 30
    restoreArgListExpectedMetadata = mkMetadata 3 3 0 15

#else
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests = const []
#endif
