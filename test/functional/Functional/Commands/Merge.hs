-- | Tests for merge command.
module Functional.Commands.Merge
  ( tests,
  )
where

import Effects.FileSystem.PathWriter (PathExistsException)
import Functional.Prelude
import SafeRm.Data.Metadata (Metadata (..))

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Merge Command"
    [ mergeSucceeds testEnv',
      mergeCollisionFails testEnv'
    ]
  where
    testEnv' = appendTestDir "merge" <$> testEnv

mergeSucceeds :: IO TestEnv -> TestTree
mergeSucceeds getTestEnv = testCase "Merge succeeds" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "mergeSucceeds" $ do
    testDir <- getTestDir

    -- SETUP SRC

    local (set' #trashDir "src") $ do
      let filesToDelete = (testDir </>) <$> ["sf1", "sf2", "sf3"]
          dirsToDelete = (testDir </>) <$> ["sdir1", "sdir2"]
      delArgList <- withSrArgsM ("delete" : filesToDelete <> dirsToDelete)

      createDirectories ((testDir </>) <$> ["sdir1", "sdir2", "sdir2/sdir3"])
      createFiles ((testDir </> "sdir2/sdir3/foo") : filesToDelete)
      assertPathsExist (filesToDelete ++ dirsToDelete)

      runSafeRm delArgList

      -- file assertions
      delTrashPathsS <- mkAllTrashPathsM ["sf1", "sf2", "sf3", "sdir1", "sdir2"]
      assertPathsExist delTrashPathsS
      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ "sf1",
            "sf2",
            "sf3",
            "sdir1",
            "sdir2"
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata

    -- SETUP DEST

    local (set' #trashDir "dest") $ do
      let filesToDelete = (testDir </>) <$> ["df1", "df2", "df3"]
          dirsToDelete = (testDir </>) <$> ["ddir1", "ddir2"]
      delArgList <- withSrArgsM ("delete" : filesToDelete <> dirsToDelete)

      createDirectories ((testDir </>) <$> ["ddir1", "ddir2", "ddir2/ddir3"])
      createFiles ((testDir </> "ddir2/ddir3/foo") : filesToDelete)
      assertPathsExist (filesToDelete ++ dirsToDelete)

      runSafeRm delArgList

      -- file assertions
      delTrashPaths <- mkAllTrashPathsM ["df1", "df2", "df3", "ddir1", "ddir2"]
      assertPathsExist delTrashPaths
      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ "df1",
            "df2",
            "df3",
            "ddir1",
            "ddir2"
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata

    -- MERGE FROM SRC TO DEST
    local (set' #trashDir "src") $ do
      let dest = testDir </> "dest"
      mergeArgs <- withSrArgsM ["merge", "-d", dest]
      runSafeRm mergeArgs

    -- VERIFY DEST
    local (set' #trashDir "dest") $ do
      -- file assertions
      mergedTrashPaths <-
        mkAllTrashPathsM
          [ "sf1",
            "sf2",
            "sf3",
            "sdir1",
            "sdir2",
            "df1",
            "df2",
            "df3",
            "ddir1",
            "ddir2"
          ]
      assertPathsExist mergedTrashPaths

      -- trash structure assertions
      mergeExpectedIdxSet <-
        mkPathDataSetM
          [ "sf1",
            "sf2",
            "sf3",
            "df1",
            "df2",
            "df3",
            "sdir1",
            "sdir2",
            "ddir1",
            "ddir2"
          ]

      (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataM

      assertSetEq mergeExpectedIdxSet mergeIdxSetDest
      mergeExpectedMetadata @=? mergeMetadataDest
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

    mergeExpectedMetadata =
      MkMetadata
        { numEntries = 10,
          numFiles = 8,
          logSize = afromInteger 0,
          size = afromInteger 0
        }

mergeCollisionFails :: IO TestEnv -> TestTree
mergeCollisionFails getTestEnv = testCase "Merge fails due to collision" $ do
  testEnv <- getTestEnv
  usingReaderT testEnv $ appendTestDirM "mergeCollisionFails" $ do
    testDir <- getTestDir

    let trashDirDest = testDir </> "dest"

    -- SETUP SRC

    (delTrashPathsSrc, pathsToDeleteSrc) <- local (set' #trashDir "src") $ do
      let filesToDelete = (testDir </>) <$> ["sf1", "sf2", "sf3"]
          dirsToDelete = (testDir </>) <$> ["sdir1", "dir2"]

      delArgList <- withSrArgsM ("delete" : filesToDelete <> dirsToDelete)
      -- NOTE: both trash dirs have dir2, giving us the collision we need
      createDirectories ((testDir </>) <$> ["sdir1", "dir2", "dir2/sdir3"])
      createFiles ((testDir </> "dir2/sdir3/foo") : filesToDelete)

      runSafeRm delArgList

      -- file assertions
      delTrashPaths <- mkAllTrashPathsM ["sf1", "sf2", "sf3", "sdir1", "dir2"]
      assertPathsExist delTrashPaths
      assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

      -- trash structure assertions
      delExpectedIdxSet <-
        mkPathDataSetM
          [ "sf1",
            "sf2",
            "sf3",
            "sdir1",
            "dir2"
          ]

      (delIdxSet, delMetadata) <- runIndexMetadataM
      assertSetEq delExpectedIdxSet delIdxSet
      delExpectedMetadata @=? delMetadata

      pure (delTrashPaths, filesToDelete ++ dirsToDelete)

    -- SETUP DEST

    (delTrashPathsDest, pathsToDeleteDest, delExpectedIdxSetDest) <-
      local (set' #trashDir "dest") $ do
        let filesToDelete = (testDir </>) <$> ["df1", "df2", "df3"]
            dirsToDelete = (testDir </>) <$> ["ddir1", "dir2"]

        delArgList <- withSrArgsM ("delete" : filesToDelete <> dirsToDelete)
        -- NOTE: both trash dirs have dir2, giving us the collision we need
        createDirectories ((testDir </>) <$> ["ddir1", "dir2", "dir2/sdir3"])
        createFiles ((testDir </> "dir2/sdir3/foo") : filesToDelete)

        runSafeRm delArgList

        -- file assertions
        delTrashPaths <- mkAllTrashPathsM ["df1", "df2", "df3", "ddir1", "dir2"]
        assertPathsExist delTrashPaths
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

        -- trash structure assertions
        delExpectedIdxSet <-
          mkPathDataSetM
            [ "df1",
              "df2",
              "df3",
              "ddir1",
              "dir2"
            ]

        (delIdxSet, delMetadata) <- runIndexMetadataM
        assertSetEq delExpectedIdxSet delIdxSet
        delExpectedMetadata @=? delMetadata

        pure (delTrashPaths, filesToDelete ++ dirsToDelete, delExpectedIdxSet)

    -- MERGE

    local (set' #trashDir "src") $ do
      mergeArgs <- withSrArgsM ["merge", "-d", trashDirDest]
      runSafeRmException @PathExistsException mergeArgs

      -- verify src unchanged
      assertPathsExist (delTrashPathsSrc ++ delTrashPathsDest)
      assertPathsDoNotExist (pathsToDeleteSrc ++ pathsToDeleteDest)

    local (set' #trashDir "dest") $ do
      -- verify dest unchanged
      (mergeIdxSetDest, mergeMetadataDest) <- runIndexMetadataM
      assertSetEq delExpectedIdxSetDest mergeIdxSetDest
      delExpectedMetadata @=? mergeMetadataDest
  where
    delExpectedMetadata =
      MkMetadata
        { numEntries = 5,
          numFiles = 4,
          logSize = afromInteger 0,
          size = afromInteger 0
        }
