{-# LANGUAGE QuasiQuotes #-}

-- | Tests for m command.
module Functional.Commands.Empty
  ( tests,
  )
where

import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Data.Metadata qualified as Metadata
import Data.HashSet qualified as HashSet
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

    -- EMPTY

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runCharon emptyArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq emptyExpectedIdxSet emptyIdxSet
    emptyExpectedMetadata @=? emptyMetadata

    assertFdoDirectorySizesM []
  where
    delExpectedMetadata = mkMetadata 8 7 0 55

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = Metadata.empty

emptyTrashTwice :: IO TestEnv -> TestTree
emptyTrashTwice getTestEnv = testCase "Calling empty twice does not error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptyTrashTwice" $ do
    emptyArgs <- withSrArgsM ["empty", "-f"]
    runCharon emptyArgs
    runCharon emptyArgs

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

    -- EMPTY

    emptyArgList <- withSrArgsM ["empty"]
    runCharon emptyArgList

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq delExpectedIdxSet emptyIdxSet
    delExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata = mkMetadata 5 5 0 25

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
    runCharon delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

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
    clearDirectory (trashDir </> Default.Utils.pathInfo)

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runCharon emptyArgList

    assertPathsExist $ fmap (trashDir </>) [Default.Utils.pathInfo, Default.Utils.pathFiles]

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq emptyExpectedIdxSet emptyIdxSet
    emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata = mkMetadata 5 4 0 35

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
    runCharon delArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

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
    clearDirectory (trashDir </> Default.Utils.pathFiles)

    emptyArgList <- withSrArgsM ["empty", "-f"]
    runCharon emptyArgList

    -- file assertions
    assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
    assertPathsExist $ fmap (trashDir </>) [Default.Utils.pathInfo, Default.Utils.pathFiles]

    -- trash structure assertions
    (emptyIdxSet, emptyMetadata) <- runIndexMetadataM
    assertSetEq emptyExpectedIdxSet emptyIdxSet
    emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedMetadata = mkMetadata 5 4 0 35

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = Metadata.empty
