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
import Data.Text qualified as T
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
emptySucceeds getTestEnv = testCase "List on empty directory succeeds" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "emptySucceeds" $ do
    argList <- withSrArgsM ["list", "--format", "m"]

    result <- captureCharon argList

    assertMatches expectedTerminal result
  where
    expectedTerminal =
      [ Exact ""
      ]

noPathsError :: IO TestEnv -> TestTree
noPathsError getTestEnv = testCase "No Paths Error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "noPathsError" $ do
    testDir <- getTestDir

    let trashDir = testDir </> pathDotTrash
    argList <- withSrArgsM ["list", "--format", "m"]

    -- setup
    clearDirectory testDir
    clearDirectory trashDir
    clearDirectory (trashDir </> Default.Utils.pathInfo)

    ex <- captureCharonException @TrashDirFilesNotFoundE argList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfix
        "The trash files directory was not found at '"
        (trashFiles <> "' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. charon empty -f).")

noInfoError :: IO TestEnv -> TestTree
noInfoError getTestEnv = testCase "No Info Error" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "noInfoError" $ do
    testDir <- getTestDir

    let trashDir = testDir </> pathDotTrash
    argList <- withSrArgsM ["list", "--format", "m"]

    -- setup
    clearDirectory testDir
    clearDirectory trashDir
    clearDirectory (trashDir </> Default.Utils.pathFiles)

    ex <- captureCharonException @TrashDirInfoNotFoundE argList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfix
        "The trash info directory was not found at '"
        (trashInfo <> "' despite the trash home existing. This can be fixed by manually creating the directory or resetting everything (i.e. charon empty -f).")

missingPathError :: IO TestEnv -> TestTree
missingPathError getTestEnv = testCase "Entry Missing Path" $ do
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
    ex <- captureCharonException @TrashEntryFileNotFoundE listArgList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfix
        "The file 'missing' was not found in the trash '"
        ( mconcat
            [ "' despite being listed in the index. This can be fixed by manually ",
              "deleting the info file or deleting everything (i.e. charon empty -f)."
            ]
        )

missingInfoError :: IO TestEnv -> TestTree
missingInfoError getTestEnv = testCase "Entry Missing Info" $ do
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

    ex <- captureCharonException @TrashEntryInfoNotFoundE argList

    assertMatch expectedErr ex
  where
    expectedErr =
      Outfix
        "The file 'bar.<ext>' was not found in the trash '"
        ( mconcat
            [ "/.trash' index despite existing in the trash itself. This can ",
              "be fixed by manually deleting the entry or deleting everything (i.e. charon empty -f)."
            ]
        )

trashFiles :: Text
trashFiles = T.pack (".trash" `cfp` "files")

trashInfo :: Text
trashInfo = T.pack (".trash" `cfp` "info")
