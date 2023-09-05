-- | Tests for m command.
module Functional.Commands.Empty
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
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

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Empty Command"
    [ emptyTrash testEnv',
      emptyTrashTwice testEnv',
      emptyNoForce testEnv',
      missingInfoForcesDelete testEnv',
      missingPathsForcesDelete testEnv'
    ]
  where
    testEnv' = appendTestDir "empty" <$> testEnv

emptyTrash :: IO TestEnv -> TestTree
emptyTrash getTestEnv = testCase "Empties trash" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "emptyTrash" $ do
    testDir <- getTestDir

    let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
        dirsToDelete = (testDir </>!) <$> ["dir1", "dir2"]
    delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

    -- setup
    -- test w/ a nested dir
    createDirectories (dirsToDelete <> [testDir </>! "dir2/dir3"])
    -- test w/ a file in dir
    createFiles ((testDir </>! "dir2/dir3/foo") : filesToDelete)
    assertPathsExist (filesToDelete ++ dirsToDelete)

    runSafeRm delArgList

    -- file assertions
    delTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir1", "dir2"]
    assertPathsExist delTrashPaths
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "dir1",
          "dir2"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- EMPTY

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgList

    -- file assertions
    emptyTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir2"]
    assertPathsDoNotExist
      ( emptyTrashPaths
          ++ filesToDelete
          ++ dirsToDelete
          ++ delTrashPaths
      )

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq emptyExpectedIdxSet emptyIdxSet
    emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = Metadata.empty

emptyTrashTwice :: IO TestEnv -> TestTree
emptyTrashTwice getTestEnv = testCase "Calling empty twice does not error" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "emptyTrashTwice" $ do
    emptyArgs <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgs
    runSafeRm emptyArgs

emptyNoForce :: IO TestEnv -> TestTree
emptyNoForce getTestEnv = testCase "Empties w/ no response deletes nothing" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "emptyNoForce" $ do
    testDir <- getTestDir
    let fileDeleteNames = show @Int <$> [1 .. 5]
        fileDeletePaths = (testDir </>!) <$> fileDeleteNames

    delArgList <- withSrArgsPathsM ["delete"] fileDeletePaths

    -- setup
    -- test w/ a file in dir
    createFiles fileDeletePaths
    assertPathsExist fileDeletePaths

    runSafeRm delArgList

    -- file assertions
    dirTrashPaths <- mkAllTrashPathsM fileDeleteNames
    assertPathsExist dirTrashPaths
    assertPathsDoNotExist fileDeletePaths

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "1",
          "2",
          "3",
          "4",
          "5"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- EMPTY

    emptyArgList <- withSrArgsM ["empty"]
    runSafeRm emptyArgList

    -- file assertions
    -- First getChar response was 'n', so files should still exist
    assertPathsExist dirTrashPaths

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet emptyIdxSet
    delExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 5,
          logSize = afromInteger 0,
          size = afromInteger 25
        }

missingInfoForcesDelete :: IO TestEnv -> TestTree
missingInfoForcesDelete getTestEnv = testCase "empty --force overwrites bad directory (no info.)" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "missingInfoForcesDelete" $ do
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
    runSafeRm delArgList

    -- file assertions
    delTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir1", "dir2"]
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
    assertPathsExist delTrashPaths

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "dir1",
          "dir2"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- delete info dir, leaving trash dir in bad state
    clearDirectory (trashDir </> pathInfo)

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgList

    -- file assertions
    emptyTrashFiles <- mkAllTrashPathsM ["f1", "f2", "f3"]
    emptyTrashDirs <- mkAllTrashPathsM ["dir2"]
    assertPathsDoNotExist (emptyTrashFiles ++ emptyTrashDirs)
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    assertPathsExist $ fmap (trashDir </>) [pathInfo, pathFiles]

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq emptyExpectedIdxSet emptyIdxSet
    emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = Metadata.empty

missingPathsForcesDelete :: IO TestEnv -> TestTree
missingPathsForcesDelete getTestEnv = testCase "empty --force overwrites bad directory (no paths/)" $ do
  testEnv <- getTestEnv
  usingTestM testEnv $ appendTestDirM "missingPathsForcesDelete" $ do
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
    runSafeRm delArgList

    -- file assertions
    delTrashPaths <- mkAllTrashPathsM ["f1", "f2", "f3", "dir1", "dir2"]
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
    assertPathsExist delTrashPaths

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ "f1",
          "f2",
          "f3",
          "dir1",
          "dir2"
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- delete info dir, leaving trash dir in bad state
    clearDirectory (trashDir </> pathFiles)

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgList

    -- file assertions
    assertPathsDoNotExist (delTrashPaths ++ filesToDelete ++ dirsToDelete)
    assertPathsExist $ fmap (trashDir </>) [pathInfo, pathFiles]

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq emptyExpectedIdxSet emptyIdxSet
    emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 20
        }

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = Metadata.empty
