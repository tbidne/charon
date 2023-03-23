-- | Tests for d command.
--
-- @since 0.1
module Functional.Commands.D
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

-- TODO: It would be nice if we could verify that the original location
-- is correct. Recently a bug was fixed as directories were using relative
-- paths. Evidently the tests did not catch this, presumably because
-- relative paths are sufficient here.

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Delete (d)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deleteDuplicateFile args,
      deletesSome args
    ]

deletesOne :: IO FilePath -> TestTree
deletesOne args = testCase "Deletes a single file" $ do
  testDir <- getTestPath args "deletesOne"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = ["d", f1, "-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  runSafeRm argList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (idxSet, metadata) <- runIndexMetadata testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/d/deletesOne/f1"
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesMany :: IO FilePath -> TestTree
deletesMany args = testCase "Deletes many paths" $ do
  testDir <- getTestPath args "deletesMany"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      argList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm argList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (idxSet, metadata) <- runIndexMetadata testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/d/deletesMany/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/d/deletesMany/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/d/deletesMany/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/d/deletesMany/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/d/deletesMany/dir2"
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deleteUnknownError :: IO FilePath -> TestTree
deleteUnknownError args = testCase "Deletes unknown prints error" $ do
  testDir <- getTestPath args "deleteUnknownError"
  let trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  (ex, _) <- captureSafeRmExceptionLogs @ExitCode argList

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (idxSet, metadata) <- runIndexMetadata testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet = HashSet.fromList []
    expectedMetadata = mempty

deleteDuplicateFile :: IO FilePath -> TestTree
deleteDuplicateFile args = testCase "Deletes duplicate file" $ do
  testDir <- getTestPath args "deleteDuplicateFile"
  let trashDir = testDir </> ".trash"
      file = testDir </> "f1"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- create and delete twice
  createFiles [file]
  assertFilesExist [file]
  runSafeRm argList

  createFiles [file]
  assertFilesExist [file]
  runSafeRm argList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1 (1)", "f1"]
  assertFilesDoNotExist [file]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (idxSet, metadata) <- runIndexMetadata testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/d/deleteDuplicateFile/f1",
          mkPathData PathTypeFile "f1 (1)" "/safe-rm/functional/d/deleteDuplicateFile/f1"
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 2,
          numFiles = 2,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesSome :: IO FilePath -> TestTree
deletesSome args = testCase "Deletes some files with errors" $ do
  testDir <- getTestPath args "deletesSome"
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryDelete = (testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]
      argList = ("d" : filesTryDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  (ex, _) <- captureSafeRmExceptionLogs @ExitCode argList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist $ mkTrashPaths trashDir ["f3", "f4"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (idxSet, metadata) <- runIndexMetadata testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/d/deletesSome/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/d/deletesSome/f2",
          mkPathData PathTypeFile "f5" "/safe-rm/functional/d/deletesSome/f5"
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot d = do
  root <- mroot
  let rdir = root </> "d"
  createDirectoryIfMissing False rdir
  pure $ rdir </> d
