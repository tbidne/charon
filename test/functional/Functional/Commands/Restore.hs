{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- | Tests for r command.
module Functional.Commands.Restore
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Effects.FileSystem.Utils (unsafeDecodeOsToFp)
import Functional.Prelude
import SafeRm.Data.Metadata
  ( Metadata
      ( MkMetadata,
        logSize,
        numEntries,
        numFiles,
        size
      ),
  )
import SafeRm.Data.Metadata qualified as Metadata
import SafeRm.Exception (RestoreCollisionE, TrashEntryNotFoundE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Restore Command"
    $ [ restoreOne testEnv',
        restoreMany testEnv',
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
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1"]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE

    restoreArgList <- withSrArgsM ["restore", "f1"]
    runSafeRm restoreArgList

    -- file assertions
    assertPathsExist [trashDir, f1]

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = Metadata.empty

restoreMany :: IO TestEnv -> TestTree
restoreMany getTestEnv = testCase "Restores several paths" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreMany" $ do
    testDir <- getTestDir

    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

    -- SETUP
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)

    assertPathsExist ((testDir </>!) <$> ["dir1", "dir2/dir3"])
    assertPathsExist ((testDir </>! "dir2/dir3/foo") : filesToDelete)

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupDirSize ["f1", "f2", "f3"] [("dir1", Nothing), ("dir2", Just "15.00B")]
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1", "f2", "f3", "dir1", "dir2"]
    (delIdxSet, delMetadata) <- runIndexMetadataM

    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE

    -- do not restore f2
    restoreArgList <- withSrArgsM ["restore", "f1", "f3", "dir1", "dir2"]
    runSafeRm restoreArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <- mkLookupSimple ["f2"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    restoreExpectedIdxSet <- mkPathDataSetM ["f2"]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

restoreUnknownError :: IO TestEnv -> TestTree
restoreUnknownError getTestEnv = testCase "Restore unknown prints error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoreUnknownError" $ do
    testDir <- getTestDir

    let f1 = testDir </>! "f1"
    delArgList <- withSrArgsPathsM ["delete"] [f1]

    -- SETUP

    -- technically we do not need to have anything in the trash to attempt
    -- a restore, but this way we can ensure the trash itself is set
    -- up (i.e. dir exists w/ index), so that we can test the restore
    -- failure only.
    clearDirectory testDir
    createFiles [f1]

    -- delete to trash first
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1"]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE
    restoreArgList <- withSrArgsM ["restore", "bad file"]
    (ex, _) <- captureSafeRmExceptionLogs @TrashEntryNotFoundE restoreArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    assertMatches expectedLookup lookupResult2

    assertMatch expectedEx ex

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet restoreIdxSet
    delExpectedMetadata @=? restoreMetadata
  where
    expectedEx =
      Outfixes
        "No entry for 'bad file'; did not find '"
        [ combineFps ["restoreUnknownError"],
          T.pack $ foldFilePaths [".trash", "info", "bad file"]
        ]
        ""
    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

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
    runSafeRm delArgList
    createFiles [f1]

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1"]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE
    restoreArgList <- withSrArgsM ["restore", "f1"]
    (ex, _) <- captureSafeRmExceptionLogs @RestoreCollisionE restoreArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    assertMatches expectedLookup lookupResult2

    assertMatch expectedEx ex

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet restoreIdxSet
    delExpectedMetadata @=? restoreMetadata
  where
    expectedEx =
      Outfixes
        "Cannot restore the trash file 'f1' as one exists at the original location: '"
        [combineFps ["restoreCollisionError"]]
        "/f1'"
    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

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
    runSafeRm (delArgList <> (unsafeDecodeOsToFp <$> [f2, f3]))
    createFiles [f1]
    runSafeRm delArgList

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupFileOpath [("f1", "f1"), ("f1 (1)", "f1"), ("f2", "f2"), ("f3", "f3")] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM2
        [ ("f1", "f1"),
          ("f1 (1)", "f1"),
          ("f2", "f2"),
          ("f3", "f3")
        ]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE
    restoreArgList <- withSrArgsM ["restore", "f1", "f1 (1)", "f2"]
    (ex, _) <- captureSafeRmExceptionLogs @RestoreCollisionE restoreArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupFileOpath [("f1 (1)", "f1"), ("f3", "f3")] []
    assertMatches expectedLookup2 lookupResult2

    assertMatch expectedEx ex

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM2
        [ ("f1 (1)", "f1"),
          ("f3", "f3")
        ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    desc = "Restore simultaneous collision prints error"
    expectedEx =
      Outfixes
        "Cannot restore the trash file 'f1 (1)' as one exists at the original location: '"
        [combineFps ["restoreSimultaneousCollisionError"]]
        "/f1'"
    delExpectedMetadata =
      MkMetadata
        { numEntries = 4,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 2,
          numFiles = 2,
          logSize = afromInteger 0,
          size = afromInteger 10
        }

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
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist realFiles

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1", "f2", "f5"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1", "f2", "f5"]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE
    restoreArgList <- withSrArgsM ("restore" : filesTryRestore)
    (ex, _) <- captureSafeRmExceptionLogs @TrashEntryNotFoundE restoreArgList

    -- file assertions
    assertPathsDoNotExist ((testDir </>!) <$> ["f3", "f4"])
    assertPathsExist ((testDir </>!) <$> ["f1", "f2", "f5"])

    assertMatch expectedEx ex

    -- trash structure assertions
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    expectedEx =
      Outfixes
        "No entry for 'f4'; did not find '"
        [ "restore/restoresSome",
          "/.trash/info/f4."
        ]
        ""
    delExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 15
        }

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = Metadata.empty

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

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToRestore ++ otherFiles)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupSimple ["1f", "1g", "2f", "2g", "3f", "3g", "f1", "f2", "f3", "g1", "g2", "g3"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "1f",
          "2f",
          "3f",
          "g1",
          "g2",
          "g3",
          "1g",
          "2g",
          "3g"
        ]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE

    -- leave g alone
    restoreArgList <- withSrArgsM ["restore", "*f*"]
    runSafeRm restoreArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupSimple ["1g", "2g", "3g", "g1", "g2", "g3"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    restoreExpectedIdxSet <-
      mkPathDataSetM
        [ "g1",
          "g2",
          "g3",
          "1g",
          "2g",
          "3g"
        ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 12,
          numFiles = 12,
          logSize = afromInteger 0,
          size = afromInteger 60
        }
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 30
        }

restoresSomeWildcards :: IO TestEnv -> TestTree
restoresSomeWildcards getTestEnv = testCase "Restores some paths via wildcards" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "restoresSomeWildcards" $ do
    testDir <- getTestDir

    let files = ["foobar", "fooBadbar", "fooXbar", "g1", "g2", "g3", "1g", "2g", "3g"]
        testFiles = (testDir </>!) <$> files

    delArgList <- withSrArgsPathsM ["delete"] testFiles

    -- SETUP
    createFiles testFiles
    assertPathsExist testFiles

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist testFiles

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupSimple ["1g", "2g", "3g", "fooBadbar", "foobar", "fooXbar", "g1", "g2", "g3"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "foobar",
          "fooBadbar",
          "fooXbar",
          "g1",
          "g2",
          "g3",
          "1g",
          "2g",
          "3g"
        ]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE

    -- We want a collision to force an error; everything should be restored
    -- from trash but fooBadBar
    createFiles [testDir </>! "fooBadbar"]

    restoreArgList <- withSrArgsM ["restore", "foo**bar", "*g*"]
    runSafeRmException @RestoreCollisionE restoreArgList

    -- file assertions
    -- 1. Everything restored but fooBarBar but that is because it already exists
    -- at original location, so everything should be there
    assertPathsExist testFiles
    -- 2. Only fooBadBar should be left in trash
    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupSimple ["fooBadbar"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    restoreExpectedIdxSet <- mkPathDataSetM ["fooBadbar"]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 9,
          numFiles = 9,
          logSize = afromInteger 0,
          size = afromInteger 45
        }
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

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

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (testWcLiteral : testFiles)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["*", "1f", "2f", "3f", "f1", "f2", "f3"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1", "f2", "f3", "1f", "2f", "3f", "*"]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE

    -- leave f alone
    restoreArgList <- withSrArgsM ["restore", "\\*"]
    runSafeRm restoreArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <- mkLookupSimple ["1f", "2f", "3f", "f1", "f2", "f3"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    restoreExpectedIdxSet <- mkPathDataSetM ["f1", "f2", "f3", "1f", "2f", "3f"]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 7,
          numFiles = 7,
          logSize = afromInteger 0,
          size = afromInteger 35
        }
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 30
        }

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

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (testWcLiterals <> testFiles)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupSimple ["yxxbar", "yxxbaz", "yxxfoo", "y*xxbar", "y*xxbaz", "y*xxfoo"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM
      [ "yxxfoo",
        "yxxbar",
        "yxxbaz",
        "y*xxfoo",
        "y*xxbar",
        "y*xxbaz"
      ]
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- RESTORE

    restoreArgList <- withSrArgsM ["restore", "y\\*xx*"]
    runSafeRm restoreArgList

    -- file assertions
    assertPathsExist testWcLiterals

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupSimple ["yxxbar", "yxxbaz", "yxxfoo"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    restoreExpectedIdxSet <- mkPathDataSetM
      [ "yxxfoo",
        "yxxbar",
        "yxxbaz"
      ]
    (restoreIdxSet, restoreMetadata) <- runIndexMetadataM
    assertSetEq restoreExpectedIdxSet restoreIdxSet
    restoreArgListExpectedMetadata @=? restoreMetadata
  where
    desc = "Restores filename w/ literal * and wildcard"

    delExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 30
        }

    restoreArgListExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 15
        }
#else
wildcardLiteralTests :: IO TestEnv -> [TestTree]
wildcardLiteralTests = const []
#endif

combineFps :: [FilePath] -> Text
combineFps =
  T.pack
    . foldFilePathsAcc "restore"
