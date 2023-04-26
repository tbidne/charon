-- | Tests for m command.
module Functional.Commands.E
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Empty (e)"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    [ emptyTrash backend args,
      emptyTrashTwice backend args,
      emptyNoForce backend args,
      missingInfoForcesDelete backend args,
      missingPathsForcesDelete backend args
    ]

emptyTrash :: Backend -> IO FilePath -> TestTree
emptyTrash backend args = testCase "Empties trash" $ do
  testDir <- getTestPath args "emptyTrash"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = withSrArgs trashDir backend ("d" : filesToDelete <> dirsToDelete)

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesExist $ mkTrashInfoPaths trashDir ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- EMPTY

  let emptyArgList = withSrArgs trashDir backend ["e", "-f"]
  runSafeRm emptyArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata' backend testDir
  assertSetEq emptyExpectedIdxSet emptyIdxSet
  emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" "/safe-rm/functional/e/emptyTrash/f1",
          mkPathData' backend PathTypeFile "f2" "/safe-rm/functional/e/emptyTrash/f2",
          mkPathData' backend PathTypeFile "f3" "/safe-rm/functional/e/emptyTrash/f3",
          mkPathData' backend PathTypeDirectory "dir1" "/safe-rm/functional/e/emptyTrash/dir1",
          mkPathData' backend PathTypeDirectory "dir2" "/safe-rm/functional/e/emptyTrash/dir2"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = mempty

emptyTrashTwice :: Backend -> IO FilePath -> TestTree
emptyTrashTwice backend args = testCase "Calling empty twice does not error" $ do
  testDir <- getTestPath args "emptyTrashTwice"
  let trashDir = testDir </> ".trash"

  runSafeRm $ withSrArgs trashDir backend ["e", "-f"]
  runSafeRm $ withSrArgs trashDir backend ["e", "-f"]

emptyNoForce :: Backend -> IO FilePath -> TestTree
emptyNoForce backend args = testCase "Empties trash without force" $ do
  testDir <- getTestPath args "emptyNoForce"
  let trashDir = testDir </> ".trash"
      fileDeleteNames = show @Int <$> [1 .. 5]
      fileDeletePaths = (testDir </>) <$> fileDeleteNames
      delArgList = withSrArgs trashDir backend $ "d" : fileDeletePaths

  -- setup
  clearDirectory testDir
  -- test w/ a file in dir
  createFiles fileDeletePaths
  assertFilesExist fileDeletePaths

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames
  assertFilesDoNotExist fileDeletePaths

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- EMPTY

  let emptyArgList = withSrArgs trashDir backend ["e"]
  runSafeRm emptyArgList

  -- file assertions
  -- First getChar response was 'n', so files should still exist
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet emptyIdxSet
  delExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "1" "/safe-rm/functional/e/emptyNoForce/1",
          mkPathData' backend PathTypeFile "2" "/safe-rm/functional/e/emptyNoForce/2",
          mkPathData' backend PathTypeFile "3" "/safe-rm/functional/e/emptyNoForce/3",
          mkPathData' backend PathTypeFile "4" "/safe-rm/functional/e/emptyNoForce/4",
          mkPathData' backend PathTypeFile "5" "/safe-rm/functional/e/emptyNoForce/5"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 5,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

missingInfoForcesDelete :: Backend -> IO FilePath -> TestTree
missingInfoForcesDelete backend args = testCase "empty --force overwrites bad directory (no info.)" $ do
  testDir <- getTestPath args "missingInfoForcesDelete"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = withSrArgs trashDir backend ("d" : filesToDelete <> dirsToDelete)

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  -- delete files
  runSafeRm delArgList

  -- file assertions
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- delete info dir, leaving trash dir in bad state
  clearDirectory (trashDir </> "info")

  let emptyArgList = withSrArgs trashDir backend ["e", "-f"]
  runSafeRm emptyArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertDirectoriesExist $ fmap (trashDir </>) ["info", "files"]

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata' backend testDir
  assertSetEq emptyExpectedIdxSet emptyIdxSet
  emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" "/safe-rm/functional/e/missingInfoForcesDelete/f1",
          mkPathData' backend PathTypeFile "f2" "/safe-rm/functional/e/missingInfoForcesDelete/f2",
          mkPathData' backend PathTypeFile "f3" "/safe-rm/functional/e/missingInfoForcesDelete/f3",
          mkPathData' backend PathTypeDirectory "dir1" "/safe-rm/functional/e/missingInfoForcesDelete/dir1",
          mkPathData' backend PathTypeDirectory "dir2" "/safe-rm/functional/e/missingInfoForcesDelete/dir2"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = mempty

missingPathsForcesDelete :: Backend -> IO FilePath -> TestTree
missingPathsForcesDelete backend args = testCase "empty --force overwrites bad directory (no paths/)" $ do
  testDir <- getTestPath args "missingPathsForcesDelete"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = withSrArgs trashDir backend ("d" : filesToDelete <> dirsToDelete)

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  -- delete files
  runSafeRm delArgList

  -- file assertions
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- delete info dir, leaving trash dir in bad state
  clearDirectory (trashDir </> "files")

  let emptyArgList = withSrArgs trashDir backend ["e", "-f"]
  runSafeRm emptyArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertDirectoriesExist $ fmap (trashDir </>) ["info", "files"]

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata' backend testDir
  assertSetEq emptyExpectedIdxSet emptyIdxSet
  emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" "/safe-rm/functional/e/missingPathsForcesDelete/f1",
          mkPathData' backend PathTypeFile "f2" "/safe-rm/functional/e/missingPathsForcesDelete/f2",
          mkPathData' backend PathTypeFile "f3" "/safe-rm/functional/e/missingPathsForcesDelete/f3",
          mkPathData' backend PathTypeDirectory "dir1" "/safe-rm/functional/e/missingPathsForcesDelete/dir1",
          mkPathData' backend PathTypeDirectory "dir2" "/safe-rm/functional/e/missingPathsForcesDelete/dir2"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    emptyExpectedIdxSet = HashSet.empty
    emptyExpectedMetadata = mempty

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "e"
