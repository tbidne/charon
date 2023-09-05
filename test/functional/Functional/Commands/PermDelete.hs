{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- | Tests for x command.
module Functional.Commands.PermDelete
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Data.Text qualified as T
import Effectful.Exception (StringException)
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
  usingTestM testEnv $ appendTestDirM "deletesOne" $ do
    testDir <- getTestDir
    let trashDir = testDir </> (testEnv ^. #trashDir)
        f1 = testDir </>! "f1"

    delArgList <- withSrArgsPathsM ["delete"] [f1]

    -- SETUP

    createFiles [f1]
    assertPathsExist [f1]

    -- delete to trash first
    runSafeRm delArgList

    -- file assertions
    delTrashFiles <- mkAllTrashPathsM ["f1"]
    assertPathsExist (trashDir : delTrashFiles)
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM

    delExpectedIdxSet <- mkPathDataSetM ["f1"]

    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    permDelArgList <- withSrArgsM ["perm-delete", "f1", "-f"]
    runSafeRm permDelArgList

    -- file assertions
    assertPathsDoNotExist $ f1 : delTrashFiles
    assertPathsExist [trashDir]

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
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
  usingTestM testEnv $ appendTestDirM "deletesMany" $ do
    testDir <- getTestDir
    let trashDir = testDir </> (testEnv ^. #trashDir)
        filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

    -- SETUP
    -- test w/ a nested dir
    createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    assertPathsExist (filesToDelete ++ dirsToDelete)

    runSafeRm delArgList

    -- file assertions
    delTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir1", "dir2"]
    assertPathsExist delTrashPaths
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "dir1",
          "dir2"
        ]

    -- trash structure assertions
    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    -- leave f2 alone
    permDelArgList <- withSrArgsM ["perm-delete", "f1", "f3", "dir1", "dir2", "-f"]
    runSafeRm permDelArgList

    -- file assertions
    permDelTrashPaths <- mkAllTrashPathsM ["f1", "f3", "dir1", "dir2"]
    notPermDelTrashFiles <- mkAllTrashPathsM ["f2"]
    assertPathsDoNotExist permDelTrashPaths
    assertPathsExist (trashDir : notPermDelTrashFiles)

    -- trash structure assertions
    permDelExpectedIdxSet <- mkPathDataSetM ["f2"]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
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
  usingTestM testEnv $ appendTestDirM "deleteUnknownError" $ do
    testDir <- getTestDir

    let trashDir = testDir </>! ".trash"
        f1 = testDir </>! "f1"

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
    delTrashFiles <- mkAllTrashPathsM ["f1"]
    assertPathsExist (trashDir : delTrashFiles)
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1"]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE
    permDelArgList <- withSrArgsM ["perm-delete", "bad file", "-f"]
    (ex, _) <- captureSafeRmExceptionLogs @TrashEntryNotFoundE permDelArgList

    -- assert exception
    assertPathsExist delTrashFiles

    assertMatch expectedEx ex

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet permDelIdxSet
    delExpectedMetadata @=? permDelMetadata
  where
    expectedEx =
      Outfixes
        "No entry for 'bad file'; did not find '"
        [ combineFps ["deleteUnknownError-"],
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

deletesSome :: IO TestEnv -> TestTree
deletesSome getTestEnv = testCase "Deletes some, errors on others" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "deletesSome" $ do
    testDir <- getTestDir

    let trashDir = testDir </>! ".trash"
        realFiles = (testDir </>!) <$> ["f1", "f2", "f5"]
        filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]

    delArgList <- withSrArgsPathsM ["delete"] realFiles

    -- setup
    createFiles realFiles
    assertPathsExist realFiles

    -- delete to trash first
    runSafeRm delArgList

    -- file assertions
    delTrashFiles <- mkAllTrashPathsM ["f1", "f2", "f5"]
    assertPathsExist (trashDir : delTrashFiles)
    assertPathsDoNotExist realFiles

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f5"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE
    permDelArgList <-
      withSrArgsM
        ("perm-delete" : filesTryPermDelete ++ ["-f"])
    (ex, _) <- captureSafeRmExceptionLogs @TrashEntryNotFoundE permDelArgList

    -- file assertions
    permDelTrashFiles <- mkAllTrashPathsM filesTryPermDelete
    assertPathsDoNotExist permDelTrashFiles

    assertMatch expectedEx ex

    -- trash structure assertions
    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
  where
    expectedEx =
      Outfixes
        "No entry for 'f4'; did not find '"
        [ combineFps ["deletesSome"],
          T.pack $ foldFilePaths [".trash", "info", "f4."]
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
  usingTestM testEnv $ appendTestDirM "deletesNoForce" $ do
    testDir <- getTestDir
    let trashDir = testDir </>! ".trash"
        fileDeleteNames = show @Int <$> [1 .. 5]
        fileDeletePaths = (testDir </>!) <$> fileDeleteNames

    delArgList <- withSrArgsPathsM ["delete"] fileDeletePaths

    -- SETUP
    createFiles fileDeletePaths
    assertPathsExist fileDeletePaths

    runSafeRm delArgList

    -- file assertions
    delTrashFiles <- mkAllTrashPathsM fileDeleteNames
    assertPathsExist delTrashFiles
    assertPathsDoNotExist fileDeletePaths

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["1", "2", "3", "4", "5"]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    permDelArgList <- withSrArgsM ("perm-delete" : fileDeleteNames)
    runSafeRm permDelArgList

    -- file assertions
    -- Our mock FuncIO alternates returning 'n' and 'y' to getChar, so without
    -- the force option we should delete 2,4 and leave 1,3,5.
    permDelTrashFiles <- mkAllTrashPathsM ["2", "4"]
    notPermDelTrashFiles <- mkAllTrashPathsM ["1", "3", "5"]
    assertPathsDoNotExist permDelTrashFiles
    assertPathsExist (trashDir : notPermDelTrashFiles)

    -- trash structure assertions
    permDelExpectedIdxSet <- mkPathDataSetM ["1", "3", "5"]

    (permDelIdxSet, permDelMetadata) <- runIndexMetadataM
    assertSetEq permDelExpectedIdxSet permDelIdxSet
    permDelExpectedMetadata @=? permDelMetadata
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
  usingTestM testEnv $ appendTestDirM "deletesWildcards" $ do
    testDir <- getTestDir
    let trashDir = testDir </>! ".trash"
        filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
        otherFiles = (testDir </>!) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]

    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> otherFiles)

    -- SETUP
    createFiles (filesToDelete <> otherFiles)
    assertPathsExist (filesToDelete ++ otherFiles)

    runSafeRm delArgList

    -- file assertions
    permDelTrashFiles <- mkAllTrashPathsM ["f1", "f2", "f3", "1f", "2f", "3f"]
    noPermDelTrashFiles <- mkAllTrashPathsM ["g1", "g2", "g3", "1g", "2g", "3g"]
    assertPathsExist (permDelTrashFiles ++ noPermDelTrashFiles)
    assertPathsDoNotExist (filesToDelete ++ otherFiles)

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

    -- PERMANENT DELETE

    -- leave g alone
    permDelArgList <- withSrArgsM ["perm-delete", "*f*", "-f"]
    runSafeRm permDelArgList

    -- file assertions
    assertPathsDoNotExist permDelTrashFiles
    assertPathsExist (trashDir : noPermDelTrashFiles)

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ "g1",
          "g2",
          "g3",
          "1g",
          "2g",
          "3g"
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
  usingTestM testEnv $ appendTestDirM "deletesSomeWildcards" $ do
    testDir <- getTestDir
    let files = ["foobar", "fooBadbar", "fooXbar", "g1", "g2", "g3", "1g", "2g", "3g"]
        testFiles = (testDir </>!) <$> files

    delArgList <- withSrArgsPathsM ["delete"] testFiles

    -- SETUP
    createFiles testFiles
    assertPathsExist testFiles

    runSafeRm delArgList

    -- file assertions
    delTrashFiles <- mkAllTrashPathsM files
    assertPathsExist delTrashFiles
    assertPathsDoNotExist testFiles

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

    -- PERMANENT DELETE

    -- NOTE: fooBadbar has been mocked in Prelude such that an attempted
    -- delete will fail. This is how this test works.
    permDelArgList <- withSrArgsM ["perm-delete", "foo**bar", "*g*", "-f"]
    runSafeRmException @StringException permDelArgList

    -- file assertions
    -- 1. Everything still gone from original location
    assertPathsDoNotExist testFiles
    -- 2. Only fooBadBar should be left in trash
    noPermDelTrashFiles <- mkAllTrashPathsM ["fooBadbar"]
    assertPathsExist noPermDelTrashFiles

    -- trash structure assertions
    permDelExpectedIdxSet <- mkPathDataSetM ["fooBadbar"]
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
  usingTestM testEnv $ appendTestDirM "deletesLiteralWildcardOnly" $ do
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
    delTrashFiles <- mkAllTrashPathsM ("*" : files)
    assertPathsExist delTrashFiles
    assertPathsDoNotExist (testWcLiteral : testFiles)

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "1f",
          "2f",
          "3f",
          "*"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    -- leave f alone
    permDelArgList <- withSrArgsM ["perm-delete", "\\*", "-f"]
    runSafeRm permDelArgList

    -- file assertions
    permDelTrashFiles <- mkAllTrashPathsM ["*"]
    noPermDelTrashFiles <- mkAllTrashPathsM files
    assertPathsDoNotExist permDelTrashFiles
    assertPathsExist noPermDelTrashFiles

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "1f",
          "2f",
          "3f"
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
  usingTestM testEnv $ appendTestDirM "deletesCombinedWildcardLiteral" $ do
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
    delTrashFiles <- mkAllTrashPathsM (wcLiterals <> files)
    assertPathsExist delTrashFiles
    assertPathsDoNotExist (testWcLiterals <> testFiles)

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "xxfoo",
          "xxbar",
          "xxbaz",
          "y*xxfoo",
          "y*xxbar",
          "y*xxbaz"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- PERMANENT DELETE

    -- leave f alone
    permDelArgList <- withSrArgsM ["perm-delete", "y\\*xx*", "-f"]
    runSafeRm permDelArgList

    -- file assertions
    permDelTrashFiles <- mkAllTrashPathsM wcLiterals
    noPermDelTrashFiles <- mkAllTrashPathsM files
    assertPathsDoNotExist permDelTrashFiles
    assertPathsExist noPermDelTrashFiles

    -- trash structure assertions
    permDelExpectedIdxSet <-
      mkPathDataSetM
        [ "xxfoo",
          "xxbar",
          "xxbaz"
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
  usingTestM testEnv $ appendTestDirM "displaysAllData" $ do
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
    delTrashFiles <- mkAllTrashPathsM ["f1"]
    assertPathsExist (trashDir : delTrashFiles)
    assertPathsDoNotExist [f1]

    -- trash structure assertions
    delExpectedIdxSet <- mkPathDataSetM ["f1"]

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
    . foldFilePathsAcc ("safe-rm" `cfp` "functional" `cfp` "perm-delete")
