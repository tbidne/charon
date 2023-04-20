-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.E
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Empty (e)"
    [ emptyTrash args,
      emptyTrashTwice args,
      emptyNoForce args,
      missingInfoForcesDelete args,
      missingPathsForcesDelete args
    ]

emptyTrash :: IO FilePath -> TestTree
emptyTrash args = testCase "Empties trash" $ do
  testDir <- getTestPath args "emptyTrash"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- EMPTY

  let emptyArgList = ["e", "-f", "-t", trashDir]
  runSafeRm emptyArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata testDir
  assertSetEq emptyExpectedIdxSet emptyIdxSet
  emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/e/emptyTrash/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/e/emptyTrash/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/e/emptyTrash/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/e/emptyTrash/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/e/emptyTrash/dir2"
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

emptyTrashTwice :: IO FilePath -> TestTree
emptyTrashTwice args = testCase "Calling empty twice does not error" $ do
  testDir <- getTestPath args "emptyTrashTwice"
  let trashDir = testDir </> ".trash"

  runSafeRm ["e", "-f", "-t", trashDir]
  runSafeRm ["e", "-f", "-t", trashDir]

emptyNoForce :: IO FilePath -> TestTree
emptyNoForce args = testCase "Empties trash without force" $ do
  testDir <- getTestPath args "emptyNoForce"
  let trashDir = testDir </> ".trash"
      fileDeleteNames = show @Int <$> [1 .. 5]
      fileDeletePaths = (testDir </>) <$> fileDeleteNames
      delArgList = ["-t", trashDir, "d"] <> fileDeletePaths

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- EMPTY

  let emptyArgList = ["-t", trashDir, "e"]
  runSafeRm emptyArgList

  -- file assertions
  -- First getChar response was 'n', so files should still exist
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet emptyIdxSet
  delExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "1" "/safe-rm/functional/e/emptyNoForce/1",
          mkPathData PathTypeFile "2" "/safe-rm/functional/e/emptyNoForce/2",
          mkPathData PathTypeFile "3" "/safe-rm/functional/e/emptyNoForce/3",
          mkPathData PathTypeFile "4" "/safe-rm/functional/e/emptyNoForce/4",
          mkPathData PathTypeFile "5" "/safe-rm/functional/e/emptyNoForce/5"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 5,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

missingInfoForcesDelete :: IO FilePath -> TestTree
missingInfoForcesDelete args = testCase "empty --force overwrites bad directory (no info.)" $ do
  testDir <- getTestPath args "missingInfoForcesDelete"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- delete info dir, leaving trash dir in bad state
  clearDirectory (trashDir </> "info")

  let emptyArgList = ["-t", trashDir, "e", "-f"]
  runSafeRm emptyArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertDirectoriesExist $ fmap (trashDir </>) ["info", "files"]

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata testDir
  assertSetEq emptyExpectedIdxSet emptyIdxSet
  emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/e/missingInfoForcesDelete/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/e/missingInfoForcesDelete/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/e/missingInfoForcesDelete/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/e/missingInfoForcesDelete/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/e/missingInfoForcesDelete/dir2"
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

missingPathsForcesDelete :: IO FilePath -> TestTree
missingPathsForcesDelete args = testCase "empty --force overwrites bad directory (no paths/)" $ do
  testDir <- getTestPath args "missingPathsForcesDelete"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- delete info dir, leaving trash dir in bad state
  clearDirectory (trashDir </> "files")

  let emptyArgList = ["-t", trashDir, "e", "-f"]
  runSafeRm emptyArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]

  assertDirectoriesExist $ fmap (trashDir </>) ["info", "files"]

  -- trash structure assertions
  (emptyIdxSet, emptyMetadata) <- runIndexMetadata testDir
  assertSetEq emptyExpectedIdxSet emptyIdxSet
  emptyExpectedMetadata @=? emptyMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/e/missingPathsForcesDelete/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/e/missingPathsForcesDelete/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/e/missingPathsForcesDelete/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/e/missingPathsForcesDelete/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/e/missingPathsForcesDelete/dir2"
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
