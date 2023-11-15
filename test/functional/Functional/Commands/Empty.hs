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
  usingReaderT testEnv $ appendTestDirM "emptyTrash" $ do
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
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "f*", "d*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <-
      mkLookupDirSize ["f1", "f2", "f3"] [("dir1", Nothing), ("dir2", Just "15.00B")]
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- EMPTY

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

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
  usingReaderT testEnv $ appendTestDirM "emptyTrashTwice" $ do
    emptyArgs <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgs
    runSafeRm emptyArgs

emptyNoForce :: IO TestEnv -> TestTree
emptyNoForce getTestEnv = testCase "Empties w/ no response deletes nothing" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptyNoForce" $ do
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
    delExpectedMetadata @=? delMetadata

    -- EMPTY

    emptyArgList <- withSrArgsM ["empty"]
    runSafeRm emptyArgList

    -- lookup assertions
    -- First getChar response was 'n', so files should still exist
    lookupResult2 <- liftIO $ captureSafeRm lookupArgs
    assertMatches expectedLookup lookupResult2

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
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupDirSize ["f1", "f2", "f3"] [("dir1", Nothing), ("dir2", Just "15.00B")]
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- delete info dir, leaving trash dir in bad state
    clearDirectory (trashDir </> pathInfo)

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgList

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
    runSafeRm delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- lookup assertions
    lookupArgs <- withSrArgsM ["lookup", "*"]
    lookupResult <- liftIO $ captureSafeRm lookupArgs
    expectedLookup <- mkLookupDirSize ["f1", "f2", "f3"] [("dir1", Nothing), ("dir2", Just "15.00B")]
    assertMatches expectedLookup lookupResult

    -- trash structure assertions
    delExpectedIdxSet <-
      mkPathDataSetM
        [ ("f1", PathTypeFile, 5),
          ("f2", PathTypeFile, 5),
          ("f3", PathTypeFile, 5),
          ("dir1", PathTypeDirectory, 5),
          ("dir2", PathTypeDirectory, 15)
        ]

    (delIdxSet, delMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet delIdxSet
    delExpectedMetadata @=? delMetadata

    -- delete info dir, leaving trash dir in bad state
    clearDirectory (trashDir </> pathFiles)

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runSafeRm emptyArgList

    -- file assertions
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
