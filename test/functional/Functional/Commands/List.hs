{-# LANGUAGE QuasiQuotes #-}

-- | Tests for l command.
module Functional.Commands.List
  ( tests,
  )
where

import Charon.Backend.Default.Exception
  ( TrashDirFilesNotFoundE,
    TrashDirInfoNotFoundE,
  )
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Exception
  ( TrashEntryFileNotFoundE,
    TrashEntryInfoNotFoundE,
  )
import Effects.FileSystem.PathWriter qualified as PW
import Functional.Prelude

-- NOTE: These tests currently rely on internal details for the trash
-- structure (see the usage of Default.Utils). If we ever get a non-compliant
-- backend, this will have to change.

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "List Command"
    [ emptySucceeds testEnv',
      noPathsError testEnv',
      noInfoError testEnv',
      missingPathError testEnv',
      missingInfoError testEnv'
    ]
  where
    testEnv' = appendTestDir "list" <$> testEnv

emptySucceeds :: IO TestEnv -> TestTree
emptySucceeds getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "List on empty directory succeeds",
        testName = testDirPrefix <> [osp|emptySucceeds|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "emptySucceeds" $ do
        testDir <- getTestDir
        argList <- withSrArgsM ["list", "--format", "m"]

        result <- captureCharon argList

        pure $ terminalToBs testDir result

noPathsError :: IO TestEnv -> TestTree
noPathsError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "No Paths Error",
        testName = testDirPrefix <> [osp|noPathsError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "noPathsError" $ do
        testDir <- getTestDir

        let trashDir = testDir </> pathDotTrash
        argList <- withSrArgsM ["list", "--format", "m"]

        -- setup
        clearDirectory testDir
        clearDirectory trashDir
        clearDirectory (trashDir </> Default.Utils.pathInfo)

        captureCharonTermBsE @TrashDirFilesNotFoundE testDir argList

noInfoError :: IO TestEnv -> TestTree
noInfoError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "No Info Error",
        testName = testDirPrefix <> [osp|noInfoError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "noInfoError" $ do
        testDir <- getTestDir

        let trashDir = testDir </> pathDotTrash
        argList <- withSrArgsM ["list", "--format", "m"]

        -- setup
        clearDirectory testDir
        clearDirectory trashDir
        clearDirectory (trashDir </> Default.Utils.pathFiles)

        captureCharonTermBsE @TrashDirInfoNotFoundE testDir argList

missingPathError :: IO TestEnv -> TestTree
missingPathError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Entry Missing Path",
        testName = testDirPrefix <> [osp|missingPathError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "missingPathError" $ do
        testDir <- getTestDir

        let trashDir = testDir </> pathDotTrash
            missing = testDir </> [osp|missing|]

        -- SETUP

        -- clearDirectory testDir
        createFilesContents [(missing, "")]

        delArgList <- withSrArgsPathsM ["delete"] [missing]
        runCharon delArgList

        -- delete file from trash for expected error
        PW.removeFile (trashDir </> Default.Utils.pathFiles </> [osp|missing|])

        -- Creating empty file so that we don't get the "size mismatch" error.
        -- We specifically want the "missing.trashinfo has no corresponding missing" error.
        createFiles [trashDir </> Default.Utils.pathFiles </> [osp|blah|]]

        listArgList <- withSrArgsM ["list", "--format", "m"]

        captureCharonTermBsE @TrashEntryFileNotFoundE testDir listArgList

missingInfoError :: IO TestEnv -> TestTree
missingInfoError getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Entry Missing Info",
        testName = testDirPrefix <> [osp|missingInfoError|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "missingInfoError" $ do
        testDir <- getTestDir

        let trashDir = testDir </> pathDotTrash
        argList <- withSrArgsM ["list", "--format", "m"]

        -- setup
        clearDirectory testDir
        clearDirectory trashDir
        clearDirectory (trashDir </> Default.Utils.pathFiles)
        clearDirectory (trashDir </> Default.Utils.pathInfo)
        createFiles [trashDir </> Default.Utils.pathFiles </> [osp|bar|]]

        captureCharonTermBsE @TrashEntryInfoNotFoundE testDir argList

testDirPrefix :: OsString
testDirPrefix = [osstr|list_|]
