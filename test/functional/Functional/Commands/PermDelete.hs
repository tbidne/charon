{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wunused-top-binds #-}

-- | Tests for x command.
module Functional.Commands.PermDelete
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
    "Permanent Delete Command"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    $ [ deletesOne backend args,
        deletesMany backend args,
        deleteUnknownError backend args,
        deletesSome backend args,
        deletesNoForce backend args,
        deletesWildcards backend args,
        deletesSomeWildcards backend args,
        displaysAllData backend args
      ]
      <> wildcardLiteralTests
  where
#if WINDOWS
    wildcardLiteralTests = []
#else
    wildcardLiteralTests =
      [ deletesLiteralWildcardOnly backend args,
        deletesCombinedWildcardLiteral backend args
      ]
#endif

deletesOne :: Backend -> IO FilePath -> TestTree
deletesOne backend args = testCase "Permanently deletes a single file" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesOne")
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

  -- PERMANENT DELETE

  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "f1", "-f"]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ f1 : mkAllTrashPaths trashDir ["f1"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.singleton
        (mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deletesOne" "f1"))

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = mempty

deletesMany :: Backend -> IO FilePath -> TestTree
deletesMany backend args = testCase "Permanently deletes several paths" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesMany")
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
  assertFilesExist filesToDelete
  assertDirectoriesExist dirsToDelete

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3"]
  assertFilesDoNotExist filesToDelete
  assertDirectoriesDoNotExist dirsToDelete
  assertDirectoriesExist $ mkTrashPaths trashDir ["", "dir1", "dir2", "dir2/dir3"]

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  -- leave f2 alone
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "f1", "f3", "dir1", "dir2", "-f"]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesToDelete
  assertDirectoriesDoNotExist $ mkTrashPaths trashDir ["dir1", "dir2/dir3"]
  assertFilesExist $ mkAllTrashPaths trashDir ["f2"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deletesMany" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "perm-delete/deletesMany" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "perm-delete/deletesMany" "f3"),
          mkPathData' backend PathTypeDirectory "dir1" (withBackendBaseDir backend "perm-delete/deletesMany" "dir1"),
          mkPathData' backend PathTypeDirectory "dir2" (withBackendBaseDir backend "perm-delete/deletesMany" "dir2")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.singleton
        (mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "perm-delete/deletesMany" "f2"))
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deleteUnknownError :: Backend -> IO FilePath -> TestTree
deleteUnknownError backend args = testCase "Delete unknown prints error" $ do
  testDir <- getTestPath args (withBackendDir backend "deleteUnknownError")
  let trashDir = testDir </> ".trash"
      f1 = testDir </> "f1"
      delArgList = withSrArgs trashDir backend ["delete", f1]

  -- SETUP

  -- technically we do not need to have anything in the trash to attempt
  -- a permanent delete, but this way we can ensure the trash itself is set
  -- up (i.e. dir exists w/ index), so that we can test the perm safe-rm
  -- failure only.
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

  -- PERMANENT DELETE
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "bad file", "-f"]
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- assert exception
  assertFilesExist $ mkAllTrashPaths trashDir ["f1"]

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet permDelIdxSet
  delExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deleteUnknownError" "f1")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesSome :: Backend -> IO FilePath -> TestTree
deletesSome backend args = testCase "Deletes some, errors on others" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesSome")
  let trashDir = testDir </> ".trash"
      realFiles = (testDir </>) <$> ["f1", "f2", "f5"]
      filesTryPermDelete = ["f1", "f2", "f3", "f4", "f5"]
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

  -- PERMANENT DELETE
  let permDelArgList =
        withSrArgs
          trashDir
          backend
          ("perm-delete" : filesTryPermDelete ++ ["-f"])
  (ex, _) <- captureSafeRmExceptionLogs @ExitCode permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesTryPermDelete

  "ExitFailure 1" @=? ex

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deletesSome" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "perm-delete/deletesSome" "f2"),
          mkPathData' backend PathTypeFile "f5" (withBackendBaseDir backend "perm-delete/deletesSome" "f5")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet = HashSet.empty
    permDelExpectedMetadata = mempty

deletesNoForce :: Backend -> IO FilePath -> TestTree
deletesNoForce backend args = testCase "Permanently deletes several paths without --force" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesNoForce")
  let trashDir = testDir </> ".trash"
      fileDeleteNames = show @Int <$> [1 .. 5]
      fileDeletePaths = (testDir </>) <$> fileDeleteNames
      delArgList = withSrArgs trashDir backend ("delete" : fileDeletePaths)

  -- SETUP
  clearDirectory testDir
  createFiles fileDeletePaths
  assertFilesExist fileDeletePaths

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir fileDeleteNames
  assertFilesDoNotExist fileDeletePaths

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  let permDelArgList = withSrArgs trashDir backend ("perm-delete" : fileDeleteNames)
  runSafeRm permDelArgList

  -- file assertions
  -- Our mock FuncIO alternates returning 'n' and 'y' to getChar, so without
  -- the force option we should delete 2,4 and leave 1,3,5.
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["2", "4"]
  assertFilesExist $ mkAllTrashPaths trashDir ["1", "3", "5"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "1" (withBackendBaseDir backend "perm-delete/deletesNoForce" "1"),
          mkPathData' backend PathTypeFile "2" (withBackendBaseDir backend "perm-delete/deletesNoForce" "2"),
          mkPathData' backend PathTypeFile "3" (withBackendBaseDir backend "perm-delete/deletesNoForce" "3"),
          mkPathData' backend PathTypeFile "4" (withBackendBaseDir backend "perm-delete/deletesNoForce" "4"),
          mkPathData' backend PathTypeFile "5" (withBackendBaseDir backend "perm-delete/deletesNoForce" "5")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 5,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "1" (withBackendBaseDir backend "perm-delete/deletesNoForce" "1"),
          mkPathData' backend PathTypeFile "3" (withBackendBaseDir backend "perm-delete/deletesNoForce" "3"),
          mkPathData' backend PathTypeFile "5" (withBackendBaseDir backend "perm-delete/deletesNoForce" "5")
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesWildcards :: Backend -> IO FilePath -> TestTree
deletesWildcards backend args = testCase "Permanently deletes several paths via wildcards" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesWildcards")
  let trashDir = testDir </> ".trash"
      filesToDelete = (testDir </>) <$> ["f1", "f2", "f3", "1f", "2f", "3f"]
      otherFiles = (testDir </>) <$> ["g1", "g2", "g3", "1g", "2g", "3g"]
      delArgList = withSrArgs trashDir backend ("delete" : filesToDelete <> otherFiles)

  -- SETUP
  clearDirectory testDir
  createFiles (filesToDelete <> otherFiles)
  assertFilesExist filesToDelete
  assertFilesExist otherFiles

  runSafeRm delArgList

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDir ["f1", "f2", "f3", "1f", "2f", "3f"]
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]
  assertFilesDoNotExist filesToDelete
  assertFilesDoNotExist otherFiles

  -- trash structure assertions
  (delIdxSet, delMetadata) <- runIndexMetadata' backend testDir
  assertSetEq delExpectedIdxSet delIdxSet
  delExpectedMetadata @=? delMetadata

  -- PERMANENT DELETE

  -- leave g alone
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "*f*", "-f"]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir filesToDelete
  assertFilesExist $ mkAllTrashPaths trashDir ["g1", "g2", "g3", "1g", "2g", "3g"]
  assertDirectoriesExist [trashDir]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deletesWildcards" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "perm-delete/deletesWildcards" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "perm-delete/deletesWildcards" "f3"),
          mkPathData' backend PathTypeFile "1f" (withBackendBaseDir backend "perm-delete/deletesWildcards" "1f"),
          mkPathData' backend PathTypeFile "2f" (withBackendBaseDir backend "perm-delete/deletesWildcards" "2f"),
          mkPathData' backend PathTypeFile "3f" (withBackendBaseDir backend "perm-delete/deletesWildcards" "3f"),
          mkPathData' backend PathTypeFile "g1" (withBackendBaseDir backend "perm-delete/deletesWildcards" "g1"),
          mkPathData' backend PathTypeFile "g2" (withBackendBaseDir backend "perm-delete/deletesWildcards" "g2"),
          mkPathData' backend PathTypeFile "g3" (withBackendBaseDir backend "perm-delete/deletesWildcards" "g3"),
          mkPathData' backend PathTypeFile "1g" (withBackendBaseDir backend "perm-delete/deletesWildcards" "1g"),
          mkPathData' backend PathTypeFile "2g" (withBackendBaseDir backend "perm-delete/deletesWildcards" "2g"),
          mkPathData' backend PathTypeFile "3g" (withBackendBaseDir backend "perm-delete/deletesWildcards" "3g")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 12,
          numFiles = 12,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "g1" (withBackendBaseDir backend "perm-delete/deletesWildcards" "g1"),
          mkPathData' backend PathTypeFile "g2" (withBackendBaseDir backend "perm-delete/deletesWildcards" "g2"),
          mkPathData' backend PathTypeFile "g3" (withBackendBaseDir backend "perm-delete/deletesWildcards" "g3"),
          mkPathData' backend PathTypeFile "1g" (withBackendBaseDir backend "perm-delete/deletesWildcards" "1g"),
          mkPathData' backend PathTypeFile "2g" (withBackendBaseDir backend "perm-delete/deletesWildcards" "2g"),
          mkPathData' backend PathTypeFile "3g" (withBackendBaseDir backend "perm-delete/deletesWildcards" "3g")
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesSomeWildcards :: Backend -> IO FilePath -> TestTree
deletesSomeWildcards backend args = testCase "Deletes some paths via wildcards" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesSomeWildcards")
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

  -- PERMANENT DELETE

  -- NOTE: fooBadbar has been mocked in Prelude such that an attempted
  -- delete will fail. This is how this test works.
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "foo**bar", "*g*", "-f"]
  runSafeRmException @ExitCode permDelArgList

  -- file assertions
  -- 1. Everything still gone from original location
  assertFilesDoNotExist testFiles
  -- 2. Only fooBadBar should be left in trash
  assertFilesExist $ mkAllTrashPaths trashDir ["fooBadbar"]

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "foobar" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "foobar"),
          mkPathData' backend PathTypeFile "fooBadbar" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "fooBadbar"),
          mkPathData' backend PathTypeFile "fooXbar" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "fooXbar"),
          mkPathData' backend PathTypeFile "g1" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "g1"),
          mkPathData' backend PathTypeFile "g2" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "g2"),
          mkPathData' backend PathTypeFile "g3" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "g3"),
          mkPathData' backend PathTypeFile "1g" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "1g"),
          mkPathData' backend PathTypeFile "2g" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "2g"),
          mkPathData' backend PathTypeFile "3g" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "3g")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 9,
          numFiles = 9,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "fooBadbar" (withBackendBaseDir backend "perm-delete/deletesSomeWildcards" "fooBadbar")
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

-- Wildcard literals are not valid in windows paths

#if !WINDOWS
deletesLiteralWildcardOnly :: Backend -> IO FilePath -> TestTree
deletesLiteralWildcardOnly backend args = testCase "Permanently deletes filename w/ literal wildcard" $ do
  testDir <- getTestPath args (withBackendDir backend "deletesLiteralWildcardOnly")
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

  -- PERMANENT DELETE

  -- leave f alone
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "\\*", "-f"]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir ["*"]
  assertFilesExist $ mkAllTrashPaths trashDir files

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "f3"),
          mkPathData' backend PathTypeFile "1f" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "1f"),
          mkPathData' backend PathTypeFile "2f" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "2f"),
          mkPathData' backend PathTypeFile "3f" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "3f"),
          mkPathData' backend PathTypeFile "*" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "*")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 7,
          numFiles = 7,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "f1"),
          mkPathData' backend PathTypeFile "f2" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "f2"),
          mkPathData' backend PathTypeFile "f3" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "f3"),
          mkPathData' backend PathTypeFile "1f" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "1f"),
          mkPathData' backend PathTypeFile "2f" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "2f"),
          mkPathData' backend PathTypeFile "3f" (withBackendBaseDir backend "perm-delete/deletesLiteralWildcardOnly" "3f")
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

deletesCombinedWildcardLiteral :: Backend -> IO FilePath -> TestTree
deletesCombinedWildcardLiteral backend args = testCase desc $ do
  testDir <- getTestPath args (withBackendDir backend "deletesCombinedWildcardLiteral")
  let trashDir = testDir </> ".trash"
      files = ["xxfoo", "xxbar", "xxbaz"]
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

  -- PERMANENT DELETE

  -- leave f alone
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "y\\*xx*", "-f"]
  runSafeRm permDelArgList

  -- file assertions
  assertFilesDoNotExist $ mkAllTrashPaths trashDir wcLiterals
  assertFilesExist $ mkAllTrashPaths trashDir files

  -- trash structure assertions
  (permDelIdxSet, permDelMetadata) <- runIndexMetadata' backend testDir
  assertSetEq permDelExpectedIdxSet permDelIdxSet
  permDelExpectedMetadata @=? permDelMetadata
  where
    desc = "Permanently deletes filename w/ literal * and wildcard"
    delExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "xxfoo" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "xxfoo"),
          mkPathData' backend PathTypeFile "xxbar" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "xxbar"),
          mkPathData' backend PathTypeFile "xxbaz" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "xxbaz"),
          mkPathData' backend PathTypeFile "y*xxfoo" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "y*xxfoo"),
          mkPathData' backend PathTypeFile "y*xxbar" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "y*xxbar"),
          mkPathData' backend PathTypeFile "y*xxbaz" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "y*xxbaz")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 6,
          numFiles = 6,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    permDelExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "xxfoo" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "xxfoo"),
          mkPathData' backend PathTypeFile "xxbar" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "xxbar"),
          mkPathData' backend PathTypeFile "xxbaz" (withBackendBaseDir backend "perm-delete/deletesCombinedWildcardLiteral" "xxbaz")
        ]
    permDelExpectedMetadata =
      MkMetadata
        { numEntries = 3,
          numFiles = 3,
          logSize = afromInteger 0,
          size = afromInteger 0
        }
#endif

displaysAllData :: Backend -> IO FilePath -> TestTree
displaysAllData backend args = testCase "Displays all data for each backend" $ do
  testDir <- getTestPath args (withBackendDir backend "displaysAllData")
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

  -- PERMANENT DELETE

  -- NOTE: we don't actually delete f1 as the first mocked answer is 'n'
  -- (see altAnswers in FuncEnv), but it doesn't matter as this test is only
  -- concerned with grabbing the terminal data for an unforced delete.
  let permDelArgList = withSrArgs trashDir backend ["perm-delete", "f1"]
  (terminalResult, _) <- captureSafeRmLogs permDelArgList

  -- assert terminal displays all data for f1
  assertMatches expectedTerminal terminalResult
  where
    expectedTerminal =
      [ Exact "",
        Exact "Type:      File",
        Exact "Name:      f1",
        Outfixes "Original:  /tmp/"
          ["/safe-rm/functional/perm-delete/displaysAllData"]
          "/f1",
        Exact "Size:      5.00B",
        Exact "Created:   2020-05-31 12:00:00",
        Exact "",
        Exact "Permanently delete (y/n)?"
      ]

    delExpectedIdxSet =
      HashSet.singleton
        (mkPathData' backend PathTypeFile "f1" (withBackendBaseDir backend "perm-delete/displaysAllData" "f1"))

    delExpectedMetadata =
      MkMetadata
        { numEntries = 1,
          numFiles = 1,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "perm-delete"
