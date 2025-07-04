{-# LANGUAGE QuasiQuotes #-}

-- | Tests for convert command.
module Functional.Commands.Convert
  ( tests,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend.Data
import Functional.Prelude

tests :: IO TestEnv -> TestTree
tests testEnv =
  testGroup
    "Convert Command"
    ((`convertsBackend` testEnv') <$> [minBound .. maxBound])
  where
    testEnv' = appendTestDir "convert" <$> testEnv

convertsBackend :: Backend -> IO TestEnv -> TestTree
convertsBackend dest getTestEnv =
  testGoldenParams
    $ MkGoldenParams
      { runner,
        testDesc = "Converts backend to " ++ destDesc,
        testName = testDirPrefix <> [osp|convertsBackend|]
      }
  where
    destDesc = Backend.Data.backendName dest
    runner = do
      testEnv <- getTestEnv
      let testDirPath =
            mconcat
              [ "convertsBackend-",
                destDesc
              ]

      usingReaderT testEnv $ appendTestDirM testDirPath $ do
        testDir <- getTestDir

        let filesToDelete = (testDir </>!) <$> ["f1", "f2", "f3"]
            dirsToDelete = (testDir </>!) <$> ["dir1", "dir2", "dir4"]
            fileLinkToDelete = testDir </> [osp|file-link|]
            dirLinkToDelete = testDir </> [osp|dir-link|]
            linksToDelete = [fileLinkToDelete, dirLinkToDelete]

        delArgList <- withSrArgsPathsM ["delete"] (filesToDelete <> dirsToDelete <> linksToDelete)

        -- setup
        clearDirectory testDir
        -- test w/ a nested dir
        createDirectories ((testDir </>!) <$> ["dir1", "dir2", "dir2/dir3", "dir4"])
        -- test w/ a file in dir
        createFiles ([testDir </>! "dir2/dir3/foo"] <> filesToDelete)
        createSymlinks [F fileLinkToDelete, D dirLinkToDelete, F $ testDir </>! "dir4" </>! "link"]
        assertPathsExist (filesToDelete ++ dirsToDelete)
        assertSymlinksExist linksToDelete

        runCharon delArgList

        -- file assertions
        assertPathsDoNotExist (filesToDelete ++ dirsToDelete)
        assertSymlinksDoNotExist linksToDelete

        -- trash structure assertions
        bs1 <- captureIndexBs testDir

        assertFdoDirectorySizesM ["dir1", "dir2", "dir4"]

        -- CONVERT

        convertArgList <- withSrArgsM ["convert", "-d", Backend.Data.backendName dest]
        runCharon convertArgList

        -- we changed the backend, so have to update our env here
        local (set' #backend dest) $ do
          -- same file assertions

          -- NOTE: [Test dir and backend changes]
          --
          -- We change the backend here as we want to test reading the trash
          -- files we just converted. This presents a problem: our usual helper
          -- functions e.g. mkAllTrashPathsM, mkPathDataSetM, runIndexMetadataTestDirM
          -- use the current backend when determining the current test directory
          -- i.e. they use getTestDir, which appends:
          --
          --      <> "-"
          --      <> Backend.Data.backendName (env ^. #backend)
          --
          -- onto the end of the path. This is normally what we want, but here
          -- it isn't, as it will calculate a new (wrong) trash directory.
          -- For instance testDir above may be
          --
          --    /tmp/charon/functional/convert/convertsBackend-(dest=cbor)-cbor
          --
          -- but after this backend change, it will be
          --
          -- /tmp/charon/functional/convert/convertsBackend-(dest=cbor)-fdo
          --
          -- This is not what we want, as the test dir has not changed! Thus we
          -- resort to variants of the aforementioned that explicitly take in
          -- the test dir: our previously calculated testDir.

          assertPathsDoNotExist (filesToDelete ++ dirsToDelete)

          -- trash structure assertions
          -- See Note [Test dir and backend changes].
          assertFdoDirectorySizesArgsM dest testDir ["dir1", "dir2", "dir4"]

        -- We capture the index the second time _outside_ of the local change
        -- above because the local change will change the directory, but we
        -- want to test the original. Hence we test the same directory here,
        -- but we need to explicitly pass in the backend since it has been
        -- changed.
        bs2 <- captureIndexBackendBs dest testDir
        pure $ bs1 `concatBs` bs2

testDirPrefix :: OsString
testDirPrefix = [osstr|convert_|]
