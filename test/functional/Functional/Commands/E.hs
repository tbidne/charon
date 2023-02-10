-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.E
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Empty (e)"
    [ emptyTrash args,
      emptyTrashTwice args,
      emptyNoForce args
    ]

emptyTrash :: IO FilePath -> TestTree
emptyTrash args = goldenVsStringDiff "Empties trash" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "e1"
      trashDir = testDir </> ".trash"
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

  -- list output assertions
  resultDel <- captureSafeRm "LIST 1" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesExist $ mkTrashInfoPaths trashDir ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- EMPTY

  let emptyArgList = ["e", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs "EMPTY" emptyArgList

  -- list output assertions
  result <- captureSafeRm "LIST 2" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "dir2"]
  assertDirectoriesDoNotExist $ mkAllTrashPaths trashDir ["dir2"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist ["", "dir1", "dir2", "dir2/dir3"]
  pure $ capturedToBs [resultDel, logs, result]
  where
    gpath = goldenPath </> "empties.golden"

emptyTrashTwice :: IO FilePath -> TestTree
emptyTrashTwice args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "e2"
      trashDir = testDir </> ".trash"

  (_, logs1) <- captureSafeRmLogs "EMPTY 1" ["e", "-f", "-t", trashDir]

  (_, logs2) <- captureSafeRmLogs "EMPTY 2" ["e", "-f", "-t", trashDir]
  pure $ capturedToBs [logs1, logs2]
  where
    desc = "Calling empty twice does not error"
    gpath = goldenPath </> "twice.golden"

emptyNoForce :: IO FilePath -> TestTree
emptyNoForce args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "e3"
      trashDir = testDir </> ".trash"
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

  -- EMPTY

  let emptyArgList = ["-t", trashDir, "e"]
  (emptyResult, emptyLogs) <-
    captureSafeRmLogs "EMPTY" emptyArgList

  -- list output assertions
  listResult <- captureSafeRm "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  -- First getChar response was 'n', so files should still exist
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames
  pure $
    capturedToBs
      [ emptyResult,
        emptyLogs,
        listResult
      ]
  where
    desc = "Empties trash without force"
    gpath = goldenPath </> "no-force.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/E"
