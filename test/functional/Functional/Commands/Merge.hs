{-# LANGUAGE QuasiQuotes #-}

-- | Tests for merge command.
module Functional.Commands.Merge
  ( tests,
  )
where

import Data.List qualified as L
import Functional.Prelude

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
mergeSucceeds getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Merge succeeds",
        testName = testDirPrefix <> [osp|mergeSucceeds|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "mergeSucceeds" $ do
        testDir <- getTestDir

        -- SETUP SRC

        idxResultsRef <- newIORef []
        local (set' #trashDir pathSrc) $ do
          let filesToDelete = (testDir </>!) <$> ["sf1", "sf2", "sf3"]
              dirsToDelete = (testDir </>!) <$> ["sdir1", "sdir2"]
          delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

          createDirectories ((testDir </>!) <$> ["sdir1", "sdir2", "sdir2/sdir3"])
          createFiles ((testDir </>! "sdir2/sdir3/foo") : filesToDelete)
          assertPathsExist (filesToDelete ++ dirsToDelete)

          runCharon delArgList

          -- file assertions
          assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

          -- trash structure assertions
          bs1 <- captureIndexBs testDir
          modifyIORef' idxResultsRef (bs1 :)
          assertFdoDirectorySizesM ["sdir1", "sdir2"]

        -- SETUP DEST

        local (set' #trashDir pathDest) $ do
          let filesToDelete = (testDir </>!) <$> ["df1", "df2", "df3"]
              dirsToDelete = (testDir </>!) <$> ["ddir1", "ddir2"]
          delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)

          createDirectories ((testDir </>!) <$> ["ddir1", "ddir2", "ddir2/ddir3"])
          createFiles ((testDir </>! "ddir2/ddir3/foo") : filesToDelete)
          assertPathsExist (filesToDelete ++ dirsToDelete)

          runCharon delArgList

          -- file assertions
          assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

          -- trash structure assertions
          bs2 <- captureIndexBs testDir
          modifyIORef' idxResultsRef (bs2 :)
          assertFdoDirectorySizesM ["ddir1", "ddir2"]

        -- MERGE FROM SRC TO DEST
        local (set' #trashDir pathSrc) $ do
          let dest = testDir </> pathDest
          mergeArgs <- withSrArgsPathsM ["merge", "-d"] [dest]
          runCharon mergeArgs

        -- VERIFY DEST
        local (set' #trashDir pathDest) $ do
          -- trash structure assertions
          bs3 <- captureIndexBs testDir
          modifyIORef' idxResultsRef (bs3 :)
          assertFdoDirectorySizesTestDirM testDir ["sdir1", "sdir2", "ddir1", "ddir2"]

        idxResults <- readIORef idxResultsRef
        pure $ foldl' concatBs "" (L.reverse idxResults)

mergeCollisionFails :: IO TestEnv -> TestTree
mergeCollisionFails getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Merge fails due to collision",
        testName = testDirPrefix <> [osp|mergeCollisionFails|]
      }
  where
    runner = do
      testEnv <- getTestEnv
      usingReaderT testEnv $ appendTestDirM "mergeCollisionFails" $ do
        testDir <- getTestDir

        let trashDirDest = testDir </>! "dest"

        -- SETUP SRC

        idxResultsRef <- newIORef []
        pathsToDeleteSrc <- local (set' #trashDir pathSrc) $ do
          let filesToDelete = (testDir </>!) <$> ["sf1", "sf2", "sf3"]
              dirsToDelete = (testDir </>!) <$> ["sdir1", "dir2"]

          delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)
          -- NOTE: both trash dirs have dir2, giving us the collision we need
          createDirectories ((testDir </>!) <$> ["sdir1", "dir2", "dir2/sdir3"])
          createFiles ((testDir </>! "dir2/sdir3/foo") : filesToDelete)

          runCharon delArgList

          -- file assertions
          assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

          -- trash structure assertions
          bs1 <- captureIndexBs testDir
          modifyIORef' idxResultsRef (bs1 :)
          assertFdoDirectorySizesM ["sdir1", "dir2"]

          pure (filesToDelete ++ dirsToDelete)

        -- SETUP DEST

        pathsToDeleteDest <-
          local (set' #trashDir pathDest) $ do
            let filesToDelete = (testDir </>!) <$> ["df1", "df2", "df3"]
                dirsToDelete = (testDir </>!) <$> ["ddir1", "dir2"]

            delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete)
            -- NOTE: both trash dirs have dir2, giving us the collision we need
            createDirectories ((testDir </>!) <$> ["ddir1", "dir2", "dir2/sdir3"])
            createFiles ((testDir </>! "dir2/sdir3/foo") : filesToDelete)

            runCharon delArgList

            -- file assertions
            assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

            -- trash structure assertions
            bs2 <- captureIndexBs testDir
            modifyIORef' idxResultsRef (bs2 :)
            assertFdoDirectorySizesM ["ddir1", "dir2"]

            pure (filesToDelete ++ dirsToDelete)

        -- MERGE

        local (set' #trashDir pathSrc) $ do
          mergeArgs <- withSrArgsPathsM ["merge", "-d"] [trashDirDest]

          -- NOTE: We could grab the exception output with
          -- captureCharonTermBsE, except we will have different
          -- error messages on different backends (the file extension is in
          -- the message). Hence we would want to create separate files
          -- based on the backend.
          runCharonE @IOException mergeArgs

          -- verify src unchanged
          assertPathsDoNotExist (pathsToDeleteSrc ++ pathsToDeleteDest)

        local (set' #trashDir pathDest) $ do
          bs3 <- captureIndexBs testDir
          modifyIORef' idxResultsRef (bs3 :)
          -- verify dest unchanged
          assertFdoDirectorySizesM ["ddir1", "dir2"]

          idxResults <- readIORef idxResultsRef
          pure $ foldl' concatBs "" (L.reverse idxResults)

pathSrc :: OsPath
pathSrc = [osp|src|]

pathDest :: OsPath
pathDest = [osp|dest|]

testDirPrefix :: OsString
testDirPrefix = [osstr|merge_|]
