-- | Tests for l command.
--
-- @since 0.1
module Functional.Commands.L
  ( tests,
  )
where

import Functional.Prelude
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.PathData (PathData (..))
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..))
import SafeRm.Data.Timestamp (fromText)
import SafeRm.Exception
  ( TrashInfoDirNotFoundE,
    TrashInfoNotFoundE,
    TrashPathDirNotFoundE,
    TrashPathNotFoundE,
  )

-- import Data.ByteString qualified as BS

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "List (l)"
    [ emptySucceeds args,
      noPathsError args,
      noInfoError args,
      missingPathError args,
      missingInfoError args
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

noPathsError :: IO FilePath -> TestTree
noPathsError args = goldenVsStringDiff "No Paths Error" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @TrashPathDirNotFoundE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    gpath = goldenPath </> "no-paths-error.golden"

noInfoError :: IO FilePath -> TestTree
noInfoError args = goldenVsStringDiff "No Info Error" diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l2"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "paths")

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @TrashInfoDirNotFoundE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    gpath = goldenPath </> "no-info-error.golden"

missingPathError :: IO FilePath -> TestTree
missingPathError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  ts <- fromText "2020-05-31 12:00:00"
  let testDir = tmpDir </> "l3"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]
      pd = MkPathData PathTypeFile "missing" (MkPathI $ testDir </> "missing") (afromInteger 0) ts
      -- Originally we were directly writing the string text here, as that
      -- served as a test that the actual serialization did not change. Alas,
      -- this was hard to get working with nix, so for now we are relying
      -- on directly serializing the data itself.
      missingInfo = PathData.encode pd

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "paths")
  clearDirectory (trashDir </> "info")
  createFileContents [(trashDir </> "info" </> "missing.info", missingInfo)]

  -- Creating empty file so that we don't get the "size mismatch" error.
  -- We specifically want the "missing.info has no corresponding missing" error.
  createFiles [trashDir </> "paths" </> "blah"]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @TrashPathNotFoundE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    desc = "Entry Missing Path"
    gpath = goldenPath </> "missing-path-error.golden"

missingInfoError :: IO FilePath -> TestTree
missingInfoError args = goldenVsStringDiff desc diff gpath $ do
  tmpDir <- args
  let testDir = tmpDir </> "l5"
      trashDir = testDir </> ".trash"
      argList = ["-t", trashDir, "l", "--format", "m"]

  -- setup
  clearDirectory testDir
  clearDirectory trashDir
  clearDirectory (trashDir </> "paths")
  clearDirectory (trashDir </> "info")
  createFiles [trashDir </> "paths" </> "bar"]

  (ex, logs) <-
    captureSafeRmExceptionLogs
      @TrashInfoNotFoundE
      "LIST"
      argList
  pure $ capturedToBs [ex, logs]
  where
    desc = "Entry Missing Info"
    gpath = goldenPath </> "missing-info-error.golden"

goldenPath :: FilePath
goldenPath = "test/functional/Functional/Commands/L"
