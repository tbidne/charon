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

  runSafeRm tmpDir delArgList

  -- list output assertions
  resultDel <- captureSafeRm tmpDir "LIST 1" ["l", "-t", trashDir]

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])

  -- EMPTY

  let emptyArgList = ["e", "-f", "-t", trashDir]
  (_, logs) <- captureSafeRmLogs tmpDir "EMPTY" emptyArgList

  -- list output assertions
  result <- captureSafeRm tmpDir "LIST 2" ["l", "-t", trashDir]

  -- file assertions
  assertFilesDoNotExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesDoNotExist
    ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  pure $ capturedToBs [resultDel, logs, result]
  where
    gpath = goldenPath </> "empties.golden"

emptyTrashTwice :: IO FilePath -> TestTree
emptyTrashTwice args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "e2"
      trashDir = testDir </> ".trash"

  (_, logs1) <- captureSafeRmLogs tmpDir "EMPTY 1" ["e", "-f", "-t", trashDir]

  (_, logs2) <- captureSafeRmLogs tmpDir "EMPTY 2" ["e", "-f", "-t", trashDir]
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

  runSafeRm tmpDir delArgList

  -- file assertions
  assertFilesExist ((trashDir </>) <$> ".index.csv" : fileDeleteNames)
  assertFilesDoNotExist fileDeletePaths

  -- EMPTY

  let emptyArgList = ["-t", trashDir, "e"]
  (emptyResult, emptyLogs) <-
    captureSafeRmLogs tmpDir "EMPTY" emptyArgList

  -- list output assertions
  listResult <- captureSafeRm tmpDir "LIST" ["l", "-t", trashDir]

  -- file assertions
  -- First getChar response was 'n', so files should still exist
  assertFilesExist ((trashDir </>) <$> ".index.csv" : fileDeleteNames)
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
