-- | Tests for merge command.
module Functional.Commands.Merge
  ( tests,
  )
where

import Data.HashSet qualified as HashSet
import Effects.FileSystem.PathWriter (PathExistsException)
import Functional.Prelude
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Metadata (Metadata (..))
import SafeRm.Data.PathType (PathType (..))

tests :: IO FilePath -> TestTree
tests args =
  testGroup
    "Merge Command"
    (backendTests args <$> [minBound .. maxBound])

backendTests :: IO FilePath -> Backend -> TestTree
backendTests args backend =
  testGroup
    (Backend.backendTestDesc backend)
    [ mergeSucceeds backend args,
      mergeCollisionFails backend args
    ]

mergeSucceeds :: Backend -> IO FilePath -> TestTree
mergeSucceeds backend args = testCase "Merge succeeds" $ do
  testDir <- getTestPath args (withBackendDir backend "mergeSucceeds")
  let trashDirSrc = testDir </> "src"
      filesToDeleteSrc = (testDir </>) <$> ["sf1", "sf2", "sf3"]
      dirsToDeleteSrc = (testDir </>) <$> ["sdir1", "sdir2"]
      delArgListSrc = withSrArgs trashDirSrc backend ("delete" : filesToDeleteSrc <> dirsToDeleteSrc)

      trashDirDest = testDir </> "dest"
      filesToDeleteDest = (testDir </>) <$> ["df1", "df2", "df3"]
      dirsToDeleteDest = (testDir </>) <$> ["ddir1", "ddir2"]
      delArgListDest = withSrArgs trashDirDest backend ("delete" : filesToDeleteDest <> dirsToDeleteDest)

  -- SETUP

  clearDirectory testDir
  createDirectories ((testDir </>) <$> ["sdir1", "sdir2", "sdir2/sdir3", "ddir1", "ddir2", "ddir2/ddir3"])
  createFiles ((testDir </> "sdir2/sdir3/foo") : (testDir </> "ddir2/ddir3/foo") : filesToDeleteSrc ++ filesToDeleteDest)
  assertFilesExist (filesToDeleteSrc ++ filesToDeleteDest)
  assertDirectoriesExist (dirsToDeleteSrc ++ dirsToDeleteDest)

  runSafeRm delArgListSrc
  runSafeRm delArgListDest

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDirSrc ["sf1", "sf2", "sf3"]
  assertFilesExist $ mkAllTrashPaths trashDirDest ["df1", "df2", "df3"]
  assertFilesExist $ mkTrashInfoPaths trashDirSrc ["sdir2"]
  assertFilesExist $ mkTrashInfoPaths trashDirDest ["ddir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirSrc ["sdir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["ddir2"]
  assertFilesDoNotExist (filesToDeleteSrc ++ filesToDeleteDest)
  assertDirectoriesDoNotExist (dirsToDeleteSrc ++ dirsToDeleteDest)
  assertDirectoriesExist $ mkTrashPaths trashDirSrc ["", "sdir1", "sdir2", "sdir2/sdir3"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["", "ddir1", "ddir2", "ddir2/ddir3"]

  -- trash structure assertions
  (delIdxSetSrc, delMetadataSrc) <- runIndexMetadataBackendTrash "src" backend testDir
  assertSetEq delExpectedIdxSetSrc delIdxSetSrc
  delExpectedMetadata @=? delMetadataSrc

  (delIdxSetDest, delMetadataDest) <- runIndexMetadataBackendTrash "dest" backend testDir
  assertSetEq delExpectedIdxSetDest delIdxSetDest
  delExpectedMetadata @=? delMetadataDest

  -- MERGE

  let mergeArgs = withSrArgs trashDirSrc backend ["merge", "-d", trashDirDest]
  runSafeRm mergeArgs

  assertFilesExist $ mkAllTrashPaths trashDirDest ["sf1", "sf2", "sf3", "df1", "df2", "df3"]
  assertFilesExist $ mkTrashInfoPaths trashDirDest ["sdir2", "ddir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["sdir2", "ddir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["", "sdir1", "sdir2", "sdir2/sdir3", "ddir1", "ddir2", "ddir2/ddir3"]

  (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataBackendTrash "dest" backend testDir
  assertSetEq mergeExpectedIdxSet mergeIdxSetDest
  mergeExpectedMetadata @=? mergeMetadataDest
  where
    delExpectedIdxSetSrc =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "sf1" (withBackendBaseDir backend "merge/mergeSucceeds" "sf1"),
          mkPathData' backend PathTypeFile "sf2" (withBackendBaseDir backend "merge/mergeSucceeds" "sf2"),
          mkPathData' backend PathTypeFile "sf3" (withBackendBaseDir backend "merge/mergeSucceeds" "sf3"),
          mkPathData' backend PathTypeDirectory "sdir1" (withBackendBaseDir backend "merge/mergeSucceeds" "sdir1"),
          mkPathData' backend PathTypeDirectory "sdir2" (withBackendBaseDir backend "merge/mergeSucceeds" "sdir2")
        ]

    delExpectedIdxSetDest =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "df1" (withBackendBaseDir backend "merge/mergeSucceeds" "df1"),
          mkPathData' backend PathTypeFile "df2" (withBackendBaseDir backend "merge/mergeSucceeds" "df2"),
          mkPathData' backend PathTypeFile "df3" (withBackendBaseDir backend "merge/mergeSucceeds" "df3"),
          mkPathData' backend PathTypeDirectory "ddir1" (withBackendBaseDir backend "merge/mergeSucceeds" "ddir1"),
          mkPathData' backend PathTypeDirectory "ddir2" (withBackendBaseDir backend "merge/mergeSucceeds" "ddir2")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    mergeExpectedIdxSet =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "sf1" (withBackendBaseDir backend "merge/mergeSucceeds" "sf1"),
          mkPathData' backend PathTypeFile "sf2" (withBackendBaseDir backend "merge/mergeSucceeds" "sf2"),
          mkPathData' backend PathTypeFile "sf3" (withBackendBaseDir backend "merge/mergeSucceeds" "sf3"),
          mkPathData' backend PathTypeFile "df1" (withBackendBaseDir backend "merge/mergeSucceeds" "df1"),
          mkPathData' backend PathTypeFile "df2" (withBackendBaseDir backend "merge/mergeSucceeds" "df2"),
          mkPathData' backend PathTypeFile "df3" (withBackendBaseDir backend "merge/mergeSucceeds" "df3"),
          mkPathData' backend PathTypeDirectory "sdir1" (withBackendBaseDir backend "merge/mergeSucceeds" "sdir1"),
          mkPathData' backend PathTypeDirectory "sdir2" (withBackendBaseDir backend "merge/mergeSucceeds" "sdir2"),
          mkPathData' backend PathTypeDirectory "ddir1" (withBackendBaseDir backend "merge/mergeSucceeds" "ddir1"),
          mkPathData' backend PathTypeDirectory "ddir2" (withBackendBaseDir backend "merge/mergeSucceeds" "ddir2")
        ]

    mergeExpectedMetadata =
      MkMetadata
        { numEntries = 10,
          numFiles = 8,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

mergeCollisionFails :: Backend -> IO FilePath -> TestTree
mergeCollisionFails backend args = testCase "Merge fails due to collision" $ do
  testDir <- getTestPath args (withBackendDir backend "mergeCollisionFails")
  let trashDirSrc = testDir </> "src"
      filesToDeleteSrc = (testDir </>) <$> ["sf1", "sf2", "sf3"]
      dirsToDeleteSrc = (testDir </>) <$> ["sdir1", "dir2"]
      delArgListSrc = withSrArgs trashDirSrc backend ("delete" : filesToDeleteSrc <> dirsToDeleteSrc)

      trashDirDest = testDir </> "dest"
      filesToDeleteDest = (testDir </>) <$> ["df1", "df2", "df3"]
      dirsToDeleteDest = (testDir </>) <$> ["ddir1", "dir2"]
      delArgListDest = withSrArgs trashDirDest backend ("delete" : filesToDeleteDest <> dirsToDeleteDest)

  -- SETUP

  clearDirectory testDir
  -- NOTE: both trash dirs have dir2, giving us the collision we need
  createDirectories ((testDir </>) <$> ["sdir1", "dir2", "dir2/sdir3"])
  createFiles ((testDir </> "dir2/sdir3/foo") : filesToDeleteSrc)
  assertFilesExist filesToDeleteSrc
  assertDirectoriesExist dirsToDeleteSrc

  runSafeRm delArgListSrc

  -- Need to create dir2 again since it was deleted in the first go round
  createDirectories ((testDir </>) <$> ["ddir1", "dir2", "dir2/ddir3"])
  createFiles ((testDir </> "dir2/ddir3/foo") : filesToDeleteDest)
  assertFilesExist filesToDeleteDest
  assertDirectoriesExist dirsToDeleteDest

  runSafeRm delArgListDest

  -- file assertions
  assertFilesExist $ mkAllTrashPaths trashDirSrc ["sf1", "sf2", "sf3"]
  assertFilesExist $ mkAllTrashPaths trashDirDest ["df1", "df2", "df3"]
  assertFilesExist $ mkTrashInfoPaths trashDirSrc ["dir2"]
  assertFilesExist $ mkTrashInfoPaths trashDirDest ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirSrc ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["dir2"]
  assertFilesDoNotExist (filesToDeleteSrc ++ filesToDeleteDest)
  assertDirectoriesDoNotExist (dirsToDeleteSrc ++ dirsToDeleteDest)
  assertDirectoriesExist $ mkTrashPaths trashDirSrc ["", "sdir1", "dir2", "dir2/sdir3"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["", "ddir1", "dir2", "dir2/ddir3"]

  -- trash structure assertions
  (delIdxSetSrc, delMetadataSrc) <- runIndexMetadataBackendTrash "src" backend testDir
  assertSetEq delExpectedIdxSetSrc delIdxSetSrc
  delExpectedMetadata @=? delMetadataSrc

  (delIdxSetDest, delMetadataDest) <- runIndexMetadataBackendTrash "dest" backend testDir
  assertSetEq delExpectedIdxSetDest delIdxSetDest
  delExpectedMetadata @=? delMetadataDest

  -- MERGE

  let mergeArgs = withSrArgs trashDirSrc backend ["merge", "-d", trashDirDest]
  runSafeRmException @PathExistsException mergeArgs

  assertFilesExist $ mkAllTrashPaths trashDirDest ["df1", "df2", "df3"]
  assertFilesDoNotExist $ mkAllTrashPaths trashDirDest ["sf1", "sf2", "sf3"]
  assertFilesExist $ mkTrashInfoPaths trashDirDest ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["dir2"]
  assertDirectoriesExist $ mkTrashPaths trashDirDest ["", "ddir1", "dir2", "dir2/ddir3"]
  assertDirectoriesDoNotExist $ mkTrashPaths trashDirDest ["sdir1", "dir2/sdir3"]

  (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataBackendTrash "dest" backend testDir
  assertSetEq delExpectedIdxSetDest mergeIdxSetDest
  delExpectedMetadata @=? mergeMetadataDest
  where
    delExpectedIdxSetSrc =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "sf1" (withBackendBaseDir backend "merge/mergeCollisionFails" "sf1"),
          mkPathData' backend PathTypeFile "sf2" (withBackendBaseDir backend "merge/mergeCollisionFails" "sf2"),
          mkPathData' backend PathTypeFile "sf3" (withBackendBaseDir backend "merge/mergeCollisionFails" "sf3"),
          mkPathData' backend PathTypeDirectory "sdir1" (withBackendBaseDir backend "merge/mergeCollisionFails" "sdir1"),
          mkPathData' backend PathTypeDirectory "dir2" (withBackendBaseDir backend "merge/mergeCollisionFails" "dir2")
        ]

    delExpectedIdxSetDest =
      HashSet.fromList
        [ mkPathData' backend PathTypeFile "df1" (withBackendBaseDir backend "merge/mergeCollisionFails" "df1"),
          mkPathData' backend PathTypeFile "df2" (withBackendBaseDir backend "merge/mergeCollisionFails" "df2"),
          mkPathData' backend PathTypeFile "df3" (withBackendBaseDir backend "merge/mergeCollisionFails" "df3"),
          mkPathData' backend PathTypeDirectory "ddir1" (withBackendBaseDir backend "merge/mergeCollisionFails" "ddir1"),
          mkPathData' backend PathTypeDirectory "dir2" (withBackendBaseDir backend "merge/mergeCollisionFails" "dir2")
        ]

    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

getTestPath :: IO FilePath -> FilePath -> IO String
getTestPath mroot = createTestDir mroot "merge"
