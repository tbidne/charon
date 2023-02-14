-- | Tests for m command.
--
-- @since 0.1
module Functional.Commands.M
  ( tests,
  )
where

import Functional.Prelude

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Metadata (m)"
    [ metadata args,
      empty args
    ]

metadata :: IO FilePath -> TestTree
metadata args = goldenVsStringDiff "Prints metadata" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "m1"
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
  delResult <- captureSafeRm "LIST" ["-t", trashDir, "l", "--format", "m"]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- METADATA

  let metaArgList = ["m", "-t", trashDir]
  (metadataResult, logs) <- captureSafeRmLogs "METADATA" metaArgList

  -- assert nothing changed
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  pure $ capturedToBs [delResult, metadataResult, logs]
  where
    gpath = goldenPath </> "metadata.golden"

empty :: IO FilePath -> TestTree
empty args = goldenVsStringDiff "Prints empty metadata" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "m2"
      trashDir = testDir </> ".trash"

  createDirectories [testDir, trashDir, trashDir </> "info", trashDir </> "paths"]
  createFiles [trashDir </> "log"]

  let metaArgList = ["m", "-t", trashDir]
  (result, logs) <- captureSafeRmLogs "METADATA" metaArgList

  pure $ capturedToBs [result, logs]
  where
    gpath = goldenPath </> "empty.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/M"
