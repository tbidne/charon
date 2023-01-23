-- | Tests for d command.
--
-- @since 0.1
module Functional.Commands.D
  ( tests,
  )
where

import Functional.Prelude

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
      deletesSome args,
      deletesNoTrace args
    ]

deletesOne :: IO FilePath -> TestTree
deletesOne args = goldenVsStringDiff "Deletes a single file" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "d1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = ["d", f1, "-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  (_, logs) <- captureSafeRmLogs tmpDir "DELETE" argList

  -- list output assertions
  result <- captureSafeRm tmpDir "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]
  pure $ capturedToBs [logs, result]
  where
    gpath = goldenPath </> "single.golden"

deletesMany :: IO FilePath -> TestTree
deletesMany args = goldenVsStringDiff "Deletes many paths" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "d2"
      trashDir = testDir </> ".trash"
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

  (_, logs) <- captureSafeRmLogs tmpDir "DELETE" argList

  -- list output assertions
  result <- captureSafeRm tmpDir "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  pure $ capturedToBs [logs, result]
  where
    gpath = goldenPath </> "many.golden"

deleteUnknownError :: IO FilePath -> TestTree
deleteUnknownError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "d3"
      trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ExitCode
      tmpDir
      "DELETE"
      argList

  pure $ capturedToBs [ex, logs]
  where
    desc = "Deletes unknown prints error"
    gpath = goldenPath </> "unknown.golden"

deleteDuplicateFile :: IO FilePath -> TestTree
deleteDuplicateFile args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "d4"
      trashDir = testDir </> ".trash"
      file = testDir </> "f1"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- create and delete twice
  createFiles [file]
  assertFilesExist [file]
  (_, logs1) <- captureSafeRmLogs tmpDir "LOGS1" argList

  createFiles [file]
  assertFilesExist [file]
  (_, logs2) <- captureSafeRmLogs tmpDir "LOGS2" argList

  result <- captureSafeRm tmpDir "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist
    [trashDir </> "f1 (1)", trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [file]
  assertDirectoriesExist [trashDir]
  pure $ capturedToBs [logs1, logs2, result]
  where
    desc = "Deletes duplicate file"
    gpath = goldenPath </> "duplicate.golden"

deletesSome :: IO FilePath -> TestTree
deletesSome args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "d5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryDelete = (testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]
      argList = ("d" : filesTryDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ExitCode
      tmpDir
      "DELETE"
      argList

  -- list output assertions
  resultList <- captureSafeRm tmpDir "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f5"]
    )
  assertFilesDoNotExist ((trashDir </>) <$> ["f3", "f4"])
  pure $ capturedToBs [ex, logs, resultList]
  where
    desc = "Deletes some files with errors"
    gpath = goldenPath </> "some.golden"

deletesNoTrace :: IO FilePath -> TestTree
deletesNoTrace args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "d6"
      trashDir = testDir </> ".trash"
      toDeleteNames = ["f1", "f3", "f5"]
      toDelete = fmap (testDir </>) toDeleteNames
      argList =
        ["-t", trashDir]
          <> ("d" : ((testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]))

  -- setup
  clearDirectory testDir
  createFiles toDelete
  assertFilesExist toDelete

  (ex, _) <-
    captureSafeRmExceptionLogs
      @ExitCode
      tmpDir
      "DELETE"
      argList

  -- file assertions
  assertFilesExist ((trashDir </>) <$> toDeleteNames)
  assertFilesDoNotExist toDelete
  pure $ capturedToBs [ex]
  where
    desc = "Delete failures without trace"
    gpath = goldenPath </> "no-trace.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/D"
