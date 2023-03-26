-- | Tests for r command.
--
-- @since 0.1
module Functional.Commands.R
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

-- | @since 0.1
tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Restore (r)"
    [ restoreOne args,
      restoreMany args,
      restoreUnknownError args,
      restoreCollisionError args,
      restoresSome args,
      restoresWildcards args,
      restoresSomeWildcards args,
      restoresLiteralWildcardOnly args,
      restoresCombinedWildcardLiteral args
    ]

restoreOne :: IO FilePath -> TestTree
restoreOne args = testCase "Restores a single file" $ do
  testDir <- getTestPath args "restoreOne"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList = ["r", "f1", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist [f1]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreOne/f1"
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

restoreMany :: IO FilePath -> TestTree
restoreMany args = testCase "Restores several paths" $ do
  testDir <- getTestPath args "restoreMany"
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3"]
      dirsToDelete = (testDir </>) <$> ["dir1", "dir2"]
      delArgList = ("d" : filesToDelete <> dirsToDelete) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir

  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList =
        -- do not restore f2
        ["r", "f1", "f3", "dir1", "dir2", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f3"]
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreMany/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoreMany/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/r/restoreMany/f3",
          mkPathData PathTypeDirectory "dir1" "/safe-rm/functional/r/restoreMany/dir1",
          mkPathData PathTypeDirectory "dir2" "/safe-rm/functional/r/restoreMany/dir2"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    restoreExpectedIdxSet = HashSet.singleton (mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoreMany/f2")
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoreUnknownError :: IO FilePath -> TestTree
restoreUnknownError args = testCase "Restore unknown prints error" $ do
  testDir <- getTestPath args "restoreUnknownError"
  let trashDir = testDir </> ".trash"
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
  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]
  assertFilesDoNotExist [f1]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = ["r", "bad file", "-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet restoreIdxSet
  delExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreUnknownError/f1"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoreCollisionError :: IO FilePath -> TestTree
restoreCollisionError args = testCase "Restore collision prints error" $ do
  testDir <- getTestPath args "restoreCollisionError"
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = ["d", f1, "-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = ["r", "f1", "-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet restoreIdxSet
  delExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoreCollisionError/f1"
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresSome :: IO FilePath -> TestTree
restoresSome args = testCase "Restores some, errors on others" $ do
  testDir <- getTestPath args "restoresSome"
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryRestore = ["f1", "f2", "f3", "f4", "f5"]
      delArgList = ("d" : realFiles) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE
  let restoreArgList = ("r" : filesTryRestore) <> ["-t", trashDir]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["f1", "f2", "f5"]
  assertFilesDoNotExist ((testDir </>) <$> ["f3", "f4"])
  assertFilesExist ((testDir </>) <$> ["f1", "f2", "f5"])

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoresSome/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoresSome/f2",
          mkPathData PathTypeFile "f5" "/safe-rm/functional/r/restoresSome/f5"
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

restoresWildcards :: IO FilePath -> TestTree
restoresWildcards args = testCase "Restores several paths via wildcards" $ do
  testDir <- getTestPath args "restoresWildcards"
  let trashDir = testDir </> ".trash"
      filesToRestore = (testDir </>) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
      otherFiles = (testDir </>) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]
      delArgList = ("d" : filesToRestore <> otherFiles) <> ["-t", trashDir]

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
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  -- leave g alone
  let restoreArgList = ["r", "*f*", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesExist filesToRestore
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoresWildcards/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoresWildcards/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/r/restoresWildcards/f3",
          mkPathData PathTypeFile "1f" "/safe-rm/functional/r/restoresWildcards/1f",
          mkPathData PathTypeFile "2f" "/safe-rm/functional/r/restoresWildcards/2f",
          mkPathData PathTypeFile "3f" "/safe-rm/functional/r/restoresWildcards/3f",
          mkPathData PathTypeFile "g1" "/safe-rm/functional/r/restoresWildcards/g1",
          mkPathData PathTypeFile "g2" "/safe-rm/functional/r/restoresWildcards/g2",
          mkPathData PathTypeFile "g3" "/safe-rm/functional/r/restoresWildcards/g3",
          mkPathData PathTypeFile "1g" "/safe-rm/functional/r/restoresWildcards/1g",
          mkPathData PathTypeFile "2g" "/safe-rm/functional/r/restoresWildcards/2g",
          mkPathData PathTypeFile "3g" "/safe-rm/functional/r/restoresWildcards/3g"
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
        [ mkPathData PathTypeFile "g1" "/safe-rm/functional/r/restoresWildcards/g1",
          mkPathData PathTypeFile "g2" "/safe-rm/functional/r/restoresWildcards/g2",
          mkPathData PathTypeFile "g3" "/safe-rm/functional/r/restoresWildcards/g3",
          mkPathData PathTypeFile "1g" "/safe-rm/functional/r/restoresWildcards/1g",
          mkPathData PathTypeFile "2g" "/safe-rm/functional/r/restoresWildcards/2g",
          mkPathData PathTypeFile "3g" "/safe-rm/functional/r/restoresWildcards/3g"
        ]
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresSomeWildcards :: IO FilePath -> TestTree
restoresSomeWildcards args = testCase "Restores some paths via wildcards" $ do
  testDir <- getTestPath args "restoresSomeWildcards"
  let trashDir = testDir </> ".trash"
      files = ["foobar", "fooBadbar", "fooXbar", "g1", "g2", "g3", "1g", "2g", "3g"]
      testFiles = (testDir </>) <$> files
      delArgList = ("d" : testFiles) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  createFiles testFiles
  assertFilesExist testFiles

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir files
  assertFilesDoNotExist testFiles

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  -- We want a collision to force an error; everything should be restored
  -- from trash but fooBadBar
  createFiles [testDir </> "fooBadbar"]

  let restoreArgList = ["r", "foo**bar", "*g*", "-t", trashDir]
  runSafeRmException @ExitCode restoreArgList

  -- file assertions
  -- 1. Everything restored but fooBarBar but that is because it already exists
  -- at original location, so everything should be there
  assertFilesExist testFiles
  -- 2. Only fooBadBar should be left in trash
  assertFilesExist $ mkAllTrashPaths trashDir ["fooBadbar"]

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "foobar" "/safe-rm/functional/r/restoresSomeWildcards/foobar",
          mkPathData PathTypeFile "fooBadbar" "/safe-rm/functional/r/restoresSomeWildcards/fooBadbar",
          mkPathData PathTypeFile "fooXbar" "/safe-rm/functional/r/restoresSomeWildcards/fooXbar",
          mkPathData PathTypeFile "g1" "/safe-rm/functional/r/restoresSomeWildcards/g1",
          mkPathData PathTypeFile "g2" "/safe-rm/functional/r/restoresSomeWildcards/g2",
          mkPathData PathTypeFile "g3" "/safe-rm/functional/r/restoresSomeWildcards/g3",
          mkPathData PathTypeFile "1g" "/safe-rm/functional/r/restoresSomeWildcards/1g",
          mkPathData PathTypeFile "2g" "/safe-rm/functional/r/restoresSomeWildcards/2g",
          mkPathData PathTypeFile "3g" "/safe-rm/functional/r/restoresSomeWildcards/3g"
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
        [ mkPathData PathTypeFile "fooBadbar" "/safe-rm/functional/r/restoresSomeWildcards/fooBadbar"
        ]
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresLiteralWildcardOnly :: IO FilePath -> TestTree
restoresLiteralWildcardOnly args = testCase "Restores filename w/ literal wildcard" $ do
  testDir <- getTestPath args "restoresLiteralWildcardOnly"
  let trashDir = testDir </> ".trash"
      files = ["f1", "f2", "f3", "1f", "2f", "3f"]
      testFiles = (testDir </>) <$> files
      testWcLiteral = testDir </> "*"
      delArgList = ("d" : testWcLiteral : testFiles) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  createFiles (testWcLiteral : testFiles)
  assertFilesExist (testWcLiteral : testFiles)

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ("*" : files)
  assertFilesDoNotExist (testWcLiteral : testFiles)

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  -- leave f alone
  let restoreArgList = ["r", "\\*", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["*"]
  assertFilesExist [testWcLiteral]
  assertFilesExist $ mkAllTrashPaths trashDir files

  -- trash structure assertions
  (restoreIdxSet, restoreMetadata) <- runIndexMetadata testDir
  assertSetEq restoreExpectedIdxSet restoreIdxSet
  restoreExpectedMetadata @=? restoreMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoresLiteralWildcardOnly/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoresLiteralWildcardOnly/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/r/restoresLiteralWildcardOnly/f3",
          mkPathData PathTypeFile "1f" "/safe-rm/functional/r/restoresLiteralWildcardOnly/1f",
          mkPathData PathTypeFile "2f" "/safe-rm/functional/r/restoresLiteralWildcardOnly/2f",
          mkPathData PathTypeFile "3f" "/safe-rm/functional/r/restoresLiteralWildcardOnly/3f",
          mkPathData PathTypeFile "*" "/safe-rm/functional/r/restoresLiteralWildcardOnly/*"
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
        [ mkPathData PathTypeFile "f1" "/safe-rm/functional/r/restoresLiteralWildcardOnly/f1",
          mkPathData PathTypeFile "f2" "/safe-rm/functional/r/restoresLiteralWildcardOnly/f2",
          mkPathData PathTypeFile "f3" "/safe-rm/functional/r/restoresLiteralWildcardOnly/f3",
          mkPathData PathTypeFile "1f" "/safe-rm/functional/r/restoresLiteralWildcardOnly/1f",
          mkPathData PathTypeFile "2f" "/safe-rm/functional/r/restoresLiteralWildcardOnly/2f",
          mkPathData PathTypeFile "3f" "/safe-rm/functional/r/restoresLiteralWildcardOnly/3f"
        ]
    restoreExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

restoresCombinedWildcardLiteral :: IO FilePath -> TestTree
restoresCombinedWildcardLiteral args = testCase desc $ do
  testDir <- getTestPath args "restoresCombinedWildcardLiteral"
  let trashDir = testDir </> ".trash"
      files = ["yxxfoo", "yxxbar", "yxxbaz"]
      wcLiterals = ["y*xxfoo", "y*xxbar", "y*xxbaz"]
      testFiles = (testDir </>) <$> files
      testWcLiterals = (testDir </>) <$> wcLiterals
      delArgList = ("d" : testWcLiterals <> testFiles) <> ["-t", trashDir]

  -- SETUP
  clearDirectory testDir
  createFiles (testWcLiterals <> testFiles)
  assertFilesExist (testWcLiterals <> testFiles)

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir (wcLiterals <> files)
  assertFilesDoNotExist (testWcLiterals <> testFiles)

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- RESTORE

  let restoreArgList = ["r", "y\\*xx*", "-t", trashDir]
  runSafeRm restoreArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir wcLiterals
  assertFilesExist testWcLiterals
  assertFilesExist $ mkAllTrashPaths trashDir files

  -- trash structure assertions
  (restoreArgListIdxSet, restoreArgListMetadata) <- runIndexMetadata testDir
  assertSetEq restoreArgListExpectedIdxSet restoreArgListIdxSet
  restoreArgListExpectedMetadata @=? restoreArgListMetadata
  where
    desc = "Restores filename w/ literal * and wildcard"
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData PathTypeFile "yxxfoo" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/yxxfoo",
          mkPathData PathTypeFile "yxxbar" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/yxxbar",
          mkPathData PathTypeFile "yxxbaz" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/yxxbaz",
          mkPathData PathTypeFile "y*xxfoo" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/y*xxfoo",
          mkPathData PathTypeFile "y*xxbar" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/y*xxbar",
          mkPathData PathTypeFile "y*xxbaz" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/y*xxbaz"
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
        [ mkPathData PathTypeFile "yxxfoo" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/yxxfoo",
          mkPathData PathTypeFile "yxxbar" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/yxxbar",
          mkPathData PathTypeFile "yxxbaz" "/safe-rm/functional/r/restoresCombinedWildcardLiteral/yxxbaz"
        ]
    restoreArgListExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "r"
