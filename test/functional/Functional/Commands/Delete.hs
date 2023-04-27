-- | Tests for d command.
module Functional.Commands.Delete
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

-- TODO: It would be nice if we could verify that the original location
-- is correct. Recently a bug was fixed as directories were using relative
-- paths. Evidently the tests did not catch this, presumably because
-- relative paths are sufficient here.

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Delete Command"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    [ deletesOne backend args,
      deletesMany backend args,
      deleteUnknownError backend args,
      deleteDuplicateFile backend args,
      deletesSome backend args
    ]

deletesOne :: Backend -> IO FilePath -> TestTree
deletesOne backend args = testCase "Deletes a single file" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesOne")
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = withSrArgs trashDir backend ["delete", f1]

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
  (idxSet, metadata) <- runIndexMetadata' backend testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "delete/deletesOne" "f1")
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesMany :: Backend -> IO FilePath -> TestTree
deletesMany backend args = testCase "Deletes many paths" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesMany")
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      argList = withSrArgs trashDir backend ("delete" : filesToDelete <> dirsToDelete)

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
  (idxSet, metadata) <- runIndexMetadata' backend testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "delete/deletesMany" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "delete/deletesMany" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "delete/deletesMany" "f3"),
          mkPathData' backend PathTypeDirectory "dir1" (withBackendBaseDir backend "delete/deletesMany" "dir1"),
          mkPathData' backend PathTypeDirectory "dir2" (withBackendBaseDir backend "delete/deletesMany" "dir2")
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deleteUnknownError :: Backend -> IO FilePath -> TestTree
deleteUnknownError backend args = testCase "Deletes unknown prints error" $ do
  testDir <- getTestPath args (withBackendDir backend "deleteUnknownError")
  let trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = withSrArgs trashDir backend ["delete", file]

  -- setup
  clearDirectory testDir

  (ex, _) <- captureSafeRmExceptionLogs @ExitCode argList

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (idxSet, metadata) <- runIndexMetadata' backend testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet = HashSet.fromList []
    expectedMetadata = mempty

deleteDuplicateFile :: Backend -> IO FilePath -> TestTree
deleteDuplicateFile backend args = testCase "Deletes duplicate file" $ do
  testDir <- getTestPath args (withBackendDir backend "deleteDuplicateFile")
  let trashDir = testDir </> ".trash"
      file = testDir </> "f1"
      argList = withSrArgs trashDir backend ["delete", file]

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
  (idxSet, metadata) <- runIndexMetadata' backend testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "delete/deleteDuplicateFile" "f1"),
          mkPathData' backend PathTypeFile "f1 (1)" (withBackendBaseDir backend "delete/deleteDuplicateFile" "f1")
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 2,
          numFiles = 2,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesSome :: Backend -> IO FilePath -> TestTree
deletesSome backend args = testCase "Deletes some files with errors" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesSome")
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryDelete = (testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]
      argList = withSrArgs trashDir backend ("delete" : filesTryDelete)

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
  (idxSet, metadata) <- runIndexMetadata' backend testDir

  assertSetEq expectedIdxSet idxSet
  expectedMetadata @=? metadata
  where
    expectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "delete/deletesSome" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "delete/deletesSome" "f2"),
          mkPathData' backend PathTypeFile "f5" (withBackendBaseDir backend "delete/deletesSome" "f5")
        ]

    expectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "delete"
