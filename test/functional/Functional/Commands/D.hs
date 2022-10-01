-- | Tests for d command.
--
-- @since 0.1
module Functional.Commands.D
  ( tests,
  )
where

import Data.Text qualified as T
import Functional.Prelude
import Functional.TestArgs (TestArgs (tmpDir))
import SafeRm.Exceptions (ExceptionI, ExceptionIndex (SomeExceptions))

-- | @since 0.1
tests :: IO TestArgs -> TestTree
tests args =
  testGroup
    "Delete (d)"
    [ deletesOne args,
      deletesMany args,
      deleteUnknownError args,
      deleteDuplicateFile args,
      deletesSome args
    ]

deletesOne :: IO TestArgs -> TestTree
deletesOne args = testCase "Deletes a single file" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d1"
      trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      argList = ["d", f1, "-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  runSafeRm argList

  -- list output assertions
  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist [trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]
  where
    expected =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/d1/f1",
        Prefix "Created:",
        Exact "Entries:      1",
        Exact "Total Files:  1",
        Prefix "Size:"
      ]

deletesMany :: IO TestArgs -> TestTree
deletesMany args = testCase "Deletes several paths" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d2"
      trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      argList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm argList

  -- list output assertions
  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f3", "dir2/dir3/foo"]
    )
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist ((trashDir </>) <$> ["", "dir1", "dir2", "dir2/dir3"])
  where
    expected =
      [ Exact "Type:      Directory",
        Exact "Name:      dir1",
        Outfix "Original:" "/safe-rm/d2/dir1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      Directory",
        Exact "Name:      dir2",
        Outfix "Original:" "/safe-rm/d2/dir2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/d2/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/d2/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f3",
        Outfix "Original:" "/safe-rm/d2/f3",
        Prefix "Created:",
        Exact "Entries:      5",
        Exact "Total Files:  4",
        Prefix "Size:"
      ]

deleteUnknownError :: IO TestArgs -> TestTree
deleteUnknownError args = testCase "Delete unknown prints error" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d3"
      trashDir = testDir </> ".trash"
      file = testDir </> "bad file"
      argList = ["d", file, "-t", trashDir]

  -- setup
  clearDirectory testDir

  -- assert exception
  result <-
    (runSafeRm argList $> Nothing)
      `catch` \(e :: ExceptionI SomeExceptions) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex -> assertMatches expected (T.lines . T.pack $ displayException ex)
  where
    expected =
      [ Exact "Encountered exception(s)",
        Outfix "- Path not found:" "/safe-rm/d3/bad file"
      ]

deleteDuplicateFile :: IO TestArgs -> TestTree
deleteDuplicateFile args = testCase "Deletes duplicate file" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d4"
      trashDir = testDir </> ".trash"
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

  result <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected result

  -- file assertions
  assertFilesExist
    [trashDir </> "f1 (1)", trashDir </> "f1", trashDir </> ".index.csv"]
  assertFilesDoNotExist [file]
  assertDirectoriesExist [trashDir]
  where
    expected =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/d4/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f1 (1)",
        Outfix "Original:" "/safe-rm/d4/f1",
        Prefix "Created:",
        Exact "Entries:      2",
        Exact "Total Files:  2",
        Prefix "Size:"
      ]

deletesSome :: IO TestArgs -> TestTree
deletesSome args = testCase "Deletes some, errors on others" $ do
  tmpDir <- view #tmpDir <$> args
  let testDir = tmpDir </> "d5"
      trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryDelete = (testDir </>) <$> ["f1", "f2", "f3", "f4", "f5"]
      argList = ("d" : filesTryDelete) <> ["-t", trashDir]

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  result <-
    (runSafeRm argList $> Nothing)
      `catch` \(e :: ExceptionI SomeExceptions) -> pure (Just e)
  case result of
    Nothing -> assertFailure "Expected exception"
    Just ex ->
      assertMatches
        expectedException
        (T.lines . T.pack $ displayException ex)

  -- list output assertions
  resultList <- captureSafeRm ["l", "-t", trashDir]
  assertMatches expected resultList

  -- file assertions
  assertFilesExist
    ( (trashDir </>)
        <$> [".index.csv", "f1", "f2", "f5"]
    )
  assertFilesDoNotExist ((trashDir </>) <$> ["f3", "f4"])
  where
    expectedException =
      [ Exact "Encountered exception(s)",
        Outfix "- Path not found:" "/safe-rm/d5/f3",
        Outfix "- Path not found:" "/safe-rm/d5/f4"
      ]
    expected =
      [ Exact "Type:      File",
        Exact "Name:      f1",
        Outfix "Original:" "/safe-rm/d5/f1",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f2",
        Outfix "Original:" "/safe-rm/d5/f2",
        Prefix "Created:",
        Exact "",
        Exact "Type:      File",
        Exact "Name:      f5",
        Outfix "Original:" "/safe-rm/d5/f5",
        Prefix "Created:",
        Exact "Entries:      3",
        Exact "Total Files:  3",
        Prefix "Size:"
      ]
