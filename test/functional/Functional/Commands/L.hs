-- | Tests for l command.
--
-- @since 0.1
module Functional.Commands.L
  ( tests,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Functional.Prelude
import SafeRm.Exception
  ( DuplicateIndexPathE,
    IndexSizeMismatchE,
    ReadIndexE,
    TrashPathNotFoundE,
  )

-- import Data.ByteString qualified as BS

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "List (l)"
    [ emptySucceeds args,
      readIndexError args,
      indexEntryNonExtantError args,
      indexDuplicatesError args,
      indexSizeMismatchError args,
      readIndexErrorNoTrace args
    ]

emptySucceeds :: IO FilePath -> TestTree
emptySucceeds args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let argList = ["-t", tmpDir </> "l1/.trash", "l", "--format", "m"]

  (result, logs) <- captureSafeRmLogs "LIST" argList
  pure $ capturedToBs [result, logs]
  where
    desc = "List on empty directory succeeds"
    gpath = goldenPath </> "empty.golden"

readIndexError :: IO FilePath -> TestTree
readIndexError args = goldenVsStringDiff "Read Index Error" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFileContents [(trashDir </> ".index.csv", "bad index")]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ReadIndexE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    gpath = goldenPath </> "read-error.golden"

indexEntryNonExtantError :: IO FilePath -> TestTree
indexEntryNonExtantError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l3"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]
      badFileLine =
        mconcat
          [ "file,foo,",
            Char8.pack (trashDir </> "foo"),
            ",0,2022-09-28 02:58:33"
          ]
      badIndex =
        Char8.unlines
          [ "type,name,original,size,created",
            badFileLine
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFileContents [(trashDir </> ".index.csv", badIndex)]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @TrashPathNotFoundE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    desc = "Index Entry Non-Extant Error"
    gpath = goldenPath </> "index-entry-error.golden"

indexDuplicatesError :: IO FilePath -> TestTree
indexDuplicatesError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l4"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]
      dupFile = trashDir </> "foo"
      dupFileLine =
        mconcat
          [ "file,foo,",
            Char8.pack dupFile,
            ",0,2022-09-28 02:58:33"
          ]
      badIndex =
        Char8.unlines
          [ "type,name,original,size,created",
            dupFileLine,
            dupFileLine
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFiles [dupFile]
  createFileContents [(trashDir </> ".index.csv", badIndex)]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @DuplicateIndexPathE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    desc = "Index Duplicates Error"
    gpath = goldenPath </> "duplicate-error.golden"

indexSizeMismatchError :: IO FilePath -> TestTree
indexSizeMismatchError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l5"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]
      file = trashDir </> "foo"
      fileLine =
        mconcat
          [ "file,foo,",
            Char8.pack file,
            ",0,2022-09-28 02:58:33"
          ]
      badIndex =
        Char8.unlines
          [ "type,name,original,size,created",
            fileLine
          ]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFiles [trashDir </> "bar", file]
  createFileContents [(trashDir </> ".index.csv", badIndex)]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @IndexSizeMismatchE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    desc = "Index Size Mismatch Error"
    gpath = goldenPath </> "index-size-error.golden"

readIndexErrorNoTrace :: IO FilePath -> TestTree
readIndexErrorNoTrace args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l6"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  createFileContents [(trashDir </> ".index.csv", "bad index")]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @ReadIndexE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    desc = "Read index error no trace"
    gpath = goldenPath </> "no-trace.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/L"
