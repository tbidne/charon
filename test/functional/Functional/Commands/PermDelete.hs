{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- | Tests for x command.
module Functional.Commands.PermDelete
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Effects.Exception (StringException)
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
import SafeRm.Exception (TrashEntryNotFoundE)

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Permanent Delete Command"
    $ [ deletesOne testEnv',
        deletesMany testEnv',
        deleteUnknownError testEnv',
        deletesSome testEnv',
        deletesNoForce testEnv',
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
    liftIO $ runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "f1"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM

    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    assertSetEq delExpectedIdxSet delIdxSet
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    permDelArgList <- withSrArgsM ["perm-delete", "f1", "-f"]
    liftIO $ runSafeRm permDelArgList

    -- file assertions
    assertPathsExist [trashDir]

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    liftIO $ permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = Metadata.empty

deletesMany :: IO TestEnv -> TestTree
deletesMany getTestEnv = testCase "Permanently deletes several paths" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesMany" $ do
    testDir <- getTestDir
    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

    -- SETUP
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    assertPathsExist (filesToDelete ++ dirsToDelete)

    liftIO $ runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupDirSize ["f1", "f2", "f3"] [("dir1", Nothing), ("dir2", Just "15.00B")]
    assertMatches expectedLookup lookupResult

    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15)
        ]

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    -- leave f2 alone
    permDelArgList <- withSrArgsM ["perm-delete", "f1", "f3", "dir1", "dir2", "-f"]
    liftIO $ runSafeRm permDelArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <- mkLookupSimple ["f2"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    permDelExpectedIdxSet <- mkPathDataSetM [("f2", PathTypeFile, 5)]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    liftIO $ permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

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
    -- up (i.e. dir exists w/ index), so that we can test the perm safe-rm
    -- failure only.
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
    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE
    permDelArgList <- withSrArgsM ["perm-delete", "bad file", "-f"]
    (ex, _) <- liftIO $ captureSafeRmExceptionLogs @TrashEntryNotFoundE permDelArgList

    -- assert exception
    assertMatch expectedEx ex

    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <- mkLookupSimple ["f1"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet permDelIdxSet
    liftIO $ delExpectedMetadata @=? permDelMetadata
  where
    expectedEx =
      Outfixes
        "No entry for 'bad file'; did not find index file '"
        [ combineFps ["deleteUnknownError"],
          "bad file"
        ]
        ""

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

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
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist realFiles

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1", "f2", "f5"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f5", PathTypeFile, 5)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE
    permDelArgList <-
      withSrArgsM
        ("perm-delete" : filesTryPermDelete ++ ["-f"])
    (ex, _) <- captureSafeRmExceptionLogs @TrashEntryNotFoundE permDelArgList

    assertMatch expectedEx ex

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    liftIO $ permDelExpectedMetadata @=? permDelMetadata
  where
    expectedEx =
      Outfixes
        "No entry for 'f4'; did not find index file '"
        [ combineFps ["deletesSome"],
          "f4"
        ]
        "'"
    delExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 15
        }

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = Metadata.empty

deletesNoForce :: IO TestEnv -> TestTree
deletesNoForce getTestEnv = testCase "Permanently deletes several paths without --force" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesNoForce" $ do
    testDir <- getTestDir
    let fileDeleteNames = show @Int <$> [1 .. 5]
        fileDeletePaths = (testDir </>!) <$> fileDeleteNames

    delArgList <- withSrArgsPathsM ["delete"] fileDeletePaths

    -- SETUP
    createFiles fileDeletePaths
    assertPathsExist fileDeletePaths

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist fileDeletePaths

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["1", "2", "3", "4", "5"] []
    assertMatches expectedLookup lookupResult

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
    liftIO $ delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    permDelArgList <- withSrArgsM ("perm-delete" : fileDeleteNames)
    runSafeRm permDelArgList

    -- lookup assertions
    -- Our mock FuncIO alternates returning 'n' and 'y' to getChar, so without
    -- the force option we should delete 2,4 and leave 1,3,5.
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <- mkLookupSimple ["1", "3", "5"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ ("1", PathTypeFile, 5),
          ("3", PathTypeFile, 5),
          ("5", PathTypeFile, 5)
        ]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    liftIO $ permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 5,
          logSize = afromInteger 0,
          size = afromInteger 25
        }
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 15
        }

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

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ otherFiles)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupSimple ["1f", "1g", "2f", "2g", "3f", "3g", "f1", "f2", "f3", "g1", "g2", "g3"] []
    assertMatches expectedLookup lookupResult

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

    -- PERMANENT DELETE

    -- leave g alone
    permDelArgList <- withSrArgsM ["perm-delete", "*f*", "-f"]
    runSafeRm permDelArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupSimple ["1g", "2g", "3g", "g1", "g2", "g3"] []
    assertMatches expectedLookup2 lookupResult2

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
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 12,
          numFiles = 12,
          logSize = afromInteger 0,
          size = afromInteger 60
        }

    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 30
        }

deletesSomeWildcards :: IO TestEnv -> TestTree
deletesSomeWildcards getTestEnv = testCase "Deletes some paths via wildcards" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "deletesSomeWildcards" $ do
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
        [ ("foobar", PathTypeFile, 5),
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

    -- PERMANENT DELETE

    -- NOTE: fooBadbar has been mocked in Prelude such that an attempted
    -- delete will fail. This is how this test works.
    permDelArgList <- withSrArgsM ["perm-delete", "foo**bar", "*g*", "-f"]
    runSafeRmException @StringException permDelArgList

    -- file assertions
    -- 1. Everything still gone from original location
    assertPathsDoNotExist testFiles
    -- 2. Only fooBadBar should be left in trash
    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupSimple ["fooBadbar"] []
    assertMatches expectedLookup2 lookupResult2

    -- trash structure assertions
    permDelExpectedIdxSet <- mkPathDataSetM [("fooBadbar", PathTypeFile, 5)]
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 9,
          numFiles = 9,
          logSize = afromInteger 0,
          size = afromInteger 45
        }
    permDelExpectedMetadata =
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

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (testWcLiteral : testFiles)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["*", "1f", "2f", "3f", "f1", "f2", "f3"] []
    assertMatches expectedLookup lookupResult

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

    -- PERMANENT DELETE

    -- leave f alone
    permDelArgList <- withSrArgsM ["perm-delete", "\\*", "-f"]
    runSafeRm permDelArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <- mkLookupSimple ["1f", "2f", "3f", "f1", "f2", "f3"] []
    assertMatches expectedLookup2 lookupResult2

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
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 7,
          numFiles = 7,
          logSize = afromInteger 0,
          size = afromInteger 35
        }
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 30
        }

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

    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (testWcLiterals <> testFiles)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupSimple ["xxbar", "xxbaz", "xxfoo", "y*xxbar", "y*xxbaz", "y*xxfoo"] []
    assertMatches expectedLookup lookupResult

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

    -- PERMANENT DELETE

    -- leave f alone
    permDelArgList <- withSrArgsM ["perm-delete", "y\\*xx*", "-f"]
    runSafeRm permDelArgList

    -- lookup assertions
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    expectedLookup2 <-
      mkLookupSimple ["xxbar", "xxbaz", "xxfoo"] []
    assertMatches expectedLookup2 lookupResult2

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
  where
    desc = "Permanently deletes filename w/ literal * and wildcard"

    delExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 30
        }
    permDelExpectedMetadata =
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
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist [f1]

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupSimple ["f1"] []
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM [("f1", PathTypeFile, 5)]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    -- NOTE: we don't actually delete f1 as the first mocked answer is 'n'
    -- (see altAnswers in FuncEnv), but it doesn't matter as this test is only
    -- concerned with grabbing the terminal data for an unforced delete.
    permDelArgList <- withSrArgsM ["perm-delete", "f1"]
    (terminalResult, _) <- captureSafeRmLogs permDelArgList

    -- assert terminal displays all data for f1
    assertMatches expectedTerminal terminalResult
  where
    expectedTerminal =
      [ Exact "Name:      f1",
        Outfixes
          "Original:"
          [combineFps ["displaysAllData"]]
          "/f1",
        Exact "Type:      File",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        -- Leaving off the "(y/n)?" suffix as the windows tests replaces all
        -- backslashes with forward slashes.
        Prefix "Permanently delete",
        Exact ""
      ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 5
        }

combineFps :: [FilePath] -> Text
combineFps =
  T.pack
    . foldFilePathsAcc "perm-delete"
