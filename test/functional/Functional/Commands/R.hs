-- | Tests for r command.
--
-- @since 0.1
module Functional.Commands.R
  ( tests,
  )
where

import Data.Text qualified as T
import Del.Exceptions (PathNotFoundError, RestoreCollisionError)
import Functional.Prelude
import Functional.TestArgs (TestArgs (..))

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Restore (r)"
    [ restoreOne args,
      restoreMany args,
      restoreUnknownError args,
      restoreCollisionError args
    ]

restoreOne :: IO TestArgs -> TestTree
restoreOne args = testCase "Restores a single file" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "r1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runDel delArgList

  -- list output assertions
  delResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE

  let restoreArgList = ["r", "f1", "-t", trashDir]
  runDel restoreArgList

  -- list output assertions
  restoreResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedRestore restoreResult

  -- file assertions
  assertFilesExist [f1, trashDir </> ".index.csv"]
  assertFilesDoNotExist [trashDir </> "f1"]
  assertDirectoriesExist [trashDir]
  where
    expectedDel =
      [ Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/r1/f1",
        Prefix "created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]

restoreMany :: IO TestArgs -> TestTree
restoreMany args = testCase "Restores several paths" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "r2"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)

  assertDirectoriesExist ((testDir </>) <$> ["dir1", "dir2/dir3"])
  assertFilesExist ((testDir </> "dir2/dir3/foo") : filesToDelete)

  runDel delArgList

  -- list output assertions
  resultDel <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel resultDel

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete

  -- RESTORE

  let restoreArgList =
        ["r", "f1", "f2", "f3", "dir1", "dir2", "-t", trashDir]
  runDel restoreArgList

  -- list output assertions
  restoreResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedRestore restoreResult

  -- file assertions
  assertFilesExist [trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist ((trashDir </>) <$> ["f1", "f2", "f3"])
  assertDirectoriesDoNotExist
    ((trashDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  where
    expectedDel =
      [ Exact "type:      Directory",
        Exact "name:      dir1",
        Outfix "original:" "/del/r2/dir1",
        Prefix "created:",
        Exact "",
        Exact "type:      Directory",
        Exact "name:      dir2",
        Outfix "original:" "/del/r2/dir2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/r2/f1",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f2",
        Outfix "original:" "/del/r2/f2",
        Prefix "created:",
        Exact "",
        Exact "type:      File",
        Exact "name:      f3",
        Outfix "original:" "/del/r2/f3",
        Prefix "created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Exact "Entries:      0",
        Exact "Total Files:  0",
        Prefix "Size:"
      ]

restoreUnknownError :: IO TestArgs -> TestTree
restoreUnknownError args = testCase "Restore unknown prints error" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "r3"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a restore, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the restore
  -- failure only.
  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first
  runDel delArgList

  -- list output assertions
  delResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "bad file", "-t", trashDir]

  -- assert exception
  result <-
    (runDel restoreArgList $> Nothing)
      `catch` \(e :: PathNotFoundError) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expectedRestore [T.pack $ displayException ex]
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/r3/f1",
        Prefix "created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore = [Outfix "Path not found:" "bad file"]

restoreCollisionError :: IO TestArgs -> TestTree
restoreCollisionError args = testCase "Restore collision prints error" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "r4"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first and recreate
  runDel delArgList
  createFiles [f1]

  -- list output assertions
  delResult <- captureDel ["l", "-t", trashDir]
  assertMatches expectedDel delResult

  -- file assertions
  assertFilesExist [trashDir </> "f1", f1, trashDir </> ".index.csv"]
  assertDirectoriesExist [trashDir]

  -- RESTORE
  let restoreArgList = ["r", "f1", "-t", trashDir]

  -- assert exception
  result <-
    (runDel restoreArgList $> Nothing)
      `catch` \(e :: RestoreCollisionError) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expectedRestore [T.pack $ displayException ex]
  assertFilesExist [trashDir </> "f1", f1, trashDir </> ".index.csv"]
  where
    expectedDel =
      [ Exact "type:      File",
        Exact "name:      f1",
        Outfix "original:" "/del/r4/f1",
        Prefix "created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]
    expectedRestore =
      [ Outfix
          "Cannot restore the file as one exists at the original location:"
          "/del/r4/f1"
      ]