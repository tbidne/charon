{-# LANGUAGE CPP #-}

-- | Tests for r command.
module Functional.Commands.Restore
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Restore Command"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    $ [ restoreOne backend args,
        restoreMany backend args,
        restoreUnknownError backend args,
        restoreCollisionError backend args,
        restoresSome backend args,
        restoresWildcards backend args,
        restoresSomeWildcards backend args
      ]
      <> wildcardLiteralTests
  where
#if WINDOWS
    wildcardLiteralTests = []
#else
    wildcardLiteralTests =
      [ restoresLiteralWildcardOnly backend args,
        restoresCombinedWildcardLiteral backend args
      ]
#endif

restoreOne :: Backend -> IO FilePath -> TestTree
restoreOne backend args = testCase "Restores a single file" $ do
  testDir <- getTestPath args (withBackendDir backend "restoreOne")
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = withSrArgs trashDir backend ["delete", f1]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]
  assertFilesExist [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList = withSrArgs trashDir backend ["restore", "f1"]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist [f1]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoreOne" "f1")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = mempty

restoreMany :: Backend -> IO FilePath -> TestTree
restoreMany backend args = testCase "Restores several paths" $ do
  testDir <- getTestPath args (withBackendDir backend "restoreMany")
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = withSrArgs trashDir backend ("delete" : filesToDelete <> dirsToDelete)

  -- SETUP
  clearDirectory testDir
  -- test w/ a nested dir
  createDirectories ((testDir </>) <$> ["dir1", "dir2", "dir2/dir3"])
  -- test w/ a file in dir
  createFiles ((testDir </> "dir2/dir3/foo") : filesToDelete)

  assertDirectoriesExist ((testDir </>) <$> ["dir1", "dir2/dir3"])
  assertFilesExist ((testDir </> "dir2/dir3/foo") : filesToDelete)

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertDirectoriesExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir

  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList =
        -- do not restore f2
        withSrArgs trashDir backend ["restore", "f1", "f3", "dir1", "dir2"]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f3"]
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoreMany" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "restore/restoreMany" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "restore/restoreMany" "f3"),
          mkPathData' backend PathTypeDirectory "dir1" (withBackendBaseDir backend "restore/restoreMany" "dir1"),
          mkPathData' backend PathTypeDirectory "dir2" (withBackendBaseDir backend "restore/restoreMany" "dir2")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.singleton (mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "restore/restoreMany" "f2"))
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoreUnknownError :: Backend -> IO FilePath -> TestTree
restoreUnknownError backend args = testCase "Restore unknown prints error" $ do
  testDir <- getTestPath args (withBackendDir backend "restoreUnknownError")
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = withSrArgs trashDir backend ["delete", f1]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a restore, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the restore
  -- failure only.
  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = withSrArgs trashDir backend ["restore", "bad file"]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet restoreIdxSet
  delExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoreUnknownError" "f1")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoreCollisionError :: Backend -> IO FilePath -> TestTree
restoreCollisionError backend args = testCase "Restore collision prints error" $ do
  testDir <- getTestPath args (withBackendDir backend "restoreCollisionError")
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = withSrArgs trashDir backend ["delete", f1]

  -- SETUP

  clearDirectory testDir
  createFiles [f1]

  -- delete to trash first and recreate
  runSafeRm delArgList
  createFiles [f1]

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = withSrArgs trashDir backend ["restore", "f1"]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet restoreIdxSet
  delExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoreCollisionError" "f1")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresSome :: Backend -> IO FilePath -> TestTree
restoresSome backend args = testCase "Restores some, errors on others" $ do
  testDir <- getTestPath args (withBackendDir backend "restoresSome")
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = withSrArgs trashDir backend ("delete" : realFiles)

  -- setup
  clearDirectory testDir
  createFiles realFiles
  assertFilesExist realFiles

  -- delete to trash first
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist realFiles
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = withSrArgs trashDir backend ("restore" : filesTryRestore)
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoresSome" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "restore/restoresSome" "f2"),
          mkPathData' backend PathTypeFile "f5" (withBackendBaseDir backend "restore/restoresSome" "f5")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.empty
    restoreExpectedMetadata = mempty

restoresWildcards :: Backend -> IO FilePath -> TestTree
restoresWildcards backend args = testCase "Restores several paths via wildcards" $ do
  testDir <- getTestPath args (withBackendDir backend "restoresWildcards")
  let trashDir = testDir </> ".trash"
      filesToRestore = (testDir </>) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
      otherFiles = (testDir </>) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]
      delArgList = withSrArgs trashDir backend ("delete" : filesToRestore <> otherFiles)

  -- SETUP
  clearDirectory testDir
  createFiles (filesToRestore <> otherFiles)
  assertFilesExist filesToRestore
  assertFilesExist otherFiles

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "1f", "2f", "3f"]
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]
  assertFilesDoNotExist filesToRestore
  assertFilesDoNotExist otherFiles

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  -- leave g alone
  let restoreArgList = withSrArgs trashDir backend ["restore", "*f*"]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist filesToRestore
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoresWildcards" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "restore/restoresWildcards" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "restore/restoresWildcards" "f3"),
          mkPathData' backend PathTypeFile "1f" (withBackendBaseDir backend "restore/restoresWildcards" "1f"),
          mkPathData' backend PathTypeFile "2f" (withBackendBaseDir backend "restore/restoresWildcards" "2f"),
          mkPathData' backend PathTypeFile "3f" (withBackendBaseDir backend "restore/restoresWildcards" "3f"),
          mkPathData' backend PathTypeFile "g1" (withBackendBaseDir backend "restore/restoresWildcards" "g1"),
          mkPathData' backend PathTypeFile "g2" (withBackendBaseDir backend "restore/restoresWildcards" "g2"),
          mkPathData' backend PathTypeFile "g3" (withBackendBaseDir backend "restore/restoresWildcards" "g3"),
          mkPathData' backend PathTypeFile "1g" (withBackendBaseDir backend "restore/restoresWildcards" "1g"),
          mkPathData' backend PathTypeFile "2g" (withBackendBaseDir backend "restore/restoresWildcards" "2g"),
          mkPathData' backend PathTypeFile "3g" (withBackendBaseDir backend "restore/restoresWildcards" "3g")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 12,
          numFiles = 12,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "g1" (withBackendBaseDir backend "restore/restoresWildcards" "g1"),
          mkPathData' backend PathTypeFile "g2" (withBackendBaseDir backend "restore/restoresWildcards" "g2"),
          mkPathData' backend PathTypeFile "g3" (withBackendBaseDir backend "restore/restoresWildcards" "g3"),
          mkPathData' backend PathTypeFile "1g" (withBackendBaseDir backend "restore/restoresWildcards" "1g"),
          mkPathData' backend PathTypeFile "2g" (withBackendBaseDir backend "restore/restoresWildcards" "2g"),
          mkPathData' backend PathTypeFile "3g" (withBackendBaseDir backend "restore/restoresWildcards" "3g")
        ]
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresSomeWildcards :: Backend -> IO FilePath -> TestTree
restoresSomeWildcards backend args = testCase "Restores some paths via wildcards" $ do
  testDir <- getTestPath args (withBackendDir backend "restoresSomeWildcards")
  let trashDir = testDir </> ".trash"
      files = ["foobar", "fooBadbar", "fooXbar", "g1", "g2", "g3", "1g", "2g", "3g"]
      testFiles = (testDir </>) <$> files
      delArgList = withSrArgs trashDir backend ("delete" : testFiles)

  -- SETUP
  clearDirectory testDir
  createFiles testFiles
  assertFilesExist testFiles

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir files
  assertFilesDoNotExist testFiles

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  -- We want a collision to force an error; everything should be restored
  -- from trash but fooBadBar
  createFiles [testDir </> "fooBadbar"]

  let restoreArgList = withSrArgs trashDir backend ["restore", "foo**bar", "*g*"]
  runSafeRmException @ExitCode restoreArgList

  -- file assertions
  -- 1. Everything restored but fooBarBar but that is because it already exists
  -- at original location, so everything should be there
  assertFilesExist testFiles
  -- 2. Only fooBadBar should be left in trash
  assertFilesExist $ mkAllTrashPaths trashDir ["fooBadbar"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "foobar" (withBackendBaseDir backend "restore/restoresSomeWildcards" "foobar"),
          mkPathData' backend PathTypeFile "fooBadbar" (withBackendBaseDir backend "restore/restoresSomeWildcards" "fooBadbar"),
          mkPathData' backend PathTypeFile "fooXbar" (withBackendBaseDir backend "restore/restoresSomeWildcards" "fooXbar"),
          mkPathData' backend PathTypeFile "g1" (withBackendBaseDir backend "restore/restoresSomeWildcards" "g1"),
          mkPathData' backend PathTypeFile "g2" (withBackendBaseDir backend "restore/restoresSomeWildcards" "g2"),
          mkPathData' backend PathTypeFile "g3" (withBackendBaseDir backend "restore/restoresSomeWildcards" "g3"),
          mkPathData' backend PathTypeFile "1g" (withBackendBaseDir backend "restore/restoresSomeWildcards" "1g"),
          mkPathData' backend PathTypeFile "2g" (withBackendBaseDir backend "restore/restoresSomeWildcards" "2g"),
          mkPathData' backend PathTypeFile "3g" (withBackendBaseDir backend "restore/restoresSomeWildcards" "3g")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 9,
          numFiles = 9,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "fooBadbar" (withBackendBaseDir backend "restore/restoresSomeWildcards" "fooBadbar")
        ]
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

-- Wildcard literals are not valid in windows paths

restoresLiteralWildcardOnly :: Backend -> IO FilePath -> TestTree
restoresLiteralWildcardOnly backend args = testCase "Restores filename w/ literal wildcard" $ do
  testDir <- getTestPath args (withBackendDir backend "restoresLiteralWildcardOnly")
  let trashDir = testDir </> ".trash"
      files = ["f1", "f2", "f3", "1f", "2f", "3f"]
      testFiles = (testDir </>) <$> files
      testWcLiteral = testDir </> "*"
      delArgList = withSrArgs trashDir backend ("delete" : testWcLiteral : testFiles)

  -- SETUP
  clearDirectory testDir
  createFiles (testWcLiteral : testFiles)
  assertFilesExist (testWcLiteral : testFiles)

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ("*" : files)
  assertFilesDoNotExist (testWcLiteral : testFiles)

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  -- leave f alone
  let restoreArgList = withSrArgs trashDir backend ["restore", "\\*"]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["*"]
  assertFilesExist [testWcLiteral]
  assertFilesExist $ mkAllTrashPaths trashDir files

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "f3"),
          mkPathData' backend PathTypeFile "1f" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "1f"),
          mkPathData' backend PathTypeFile "2f" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "2f"),
          mkPathData' backend PathTypeFile "3f" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "3f"),
          mkPathData' backend PathTypeFile "*" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "*")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 7,
          numFiles = 7,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "f3"),
          mkPathData' backend PathTypeFile "1f" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "1f"),
          mkPathData' backend PathTypeFile "2f" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "2f"),
          mkPathData' backend PathTypeFile "3f" (withBackendBaseDir backend "restore/restoresLiteralWildcardOnly" "3f")
        ]
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresCombinedWildcardLiteral :: Backend -> IO FilePath -> TestTree
restoresCombinedWildcardLiteral backend args = testCase desc $ do
  testDir <- getTestPath args (withBackendDir backend "restoresCombinedWildcardLiteral")
  let trashDir = testDir </> ".trash"
      files = ["yxxfoo", "yxxbar", "yxxbaz"]
      wcLiterals = ["y*xxfoo", "y*xxbar", "y*xxbaz"]
      testFiles = (testDir </>) <$> files
      testWcLiterals = (testDir </>) <$> wcLiterals
      delArgList = withSrArgs trashDir backend ("delete" : testWcLiterals <> testFiles)

  -- SETUP
  clearDirectory testDir
  createFiles (testWcLiterals <> testFiles)
  assertFilesExist (testWcLiterals <> testFiles)

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir (wcLiterals <> files)
  assertFilesDoNotExist (testWcLiterals <> testFiles)

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList = withSrArgs trashDir backend ["restore", "y\\*xx*"]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir wcLiterals
  assertFilesExist testWcLiterals
  assertFilesExist $ mkAllTrashPaths trashDir files

  -- trash structure assertions
  (restoreArgListIdxSet, restoreArgListMetadata) <- runIndexMetadata' backend testDir
  assertSetEq restoreArgListExpectedIdxSet restoreArgListIdxSet
  restoreArgListExpectedMetadata @=? restoreArgListMetadata
  where
    desc = "Restores filename w/ literal * and wildcard"
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "yxxfoo" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "yxxfoo"),
          mkPathData' backend PathTypeFile "yxxbar" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "yxxbar"),
          mkPathData' backend PathTypeFile "yxxbaz" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "yxxbaz"),
          mkPathData' backend PathTypeFile "y*xxfoo" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "y*xxfoo"),
          mkPathData' backend PathTypeFile "y*xxbar" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "y*xxbar"),
          mkPathData' backend PathTypeFile "y*xxbaz" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "y*xxbaz")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreArgListExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "yxxfoo" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "yxxfoo"),
          mkPathData' backend PathTypeFile "yxxbar" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "yxxbar"),
          mkPathData' backend PathTypeFile "yxxbaz" (withBackendBaseDir backend "restore/restoresCombinedWildcardLiteral" "yxxbaz")
        ]
    restoreArgListExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "restore"
