{-# LANGUAGE QuasiQuotes #-}

-- | Entrypoint for functional tests.
module Main (main) where

import Charon.Backend.Data (Backend (BackendCbor))
import Charon.Backend.Data qualified as Backend
import Data.Char qualified as Ch
import Effects.FileSystem.PathReader qualified as Dir
import Functional.Commands.Convert qualified as Convert
import Functional.Commands.Delete qualified as Delete
import Functional.Commands.Empty qualified as Empty
import Functional.Commands.List qualified as List
import Functional.Commands.Merge qualified as Merge
import Functional.Commands.Metadata qualified as Metadata
import Functional.Commands.PermDelete qualified as PermDelete
import Functional.Commands.Restore qualified as Restore
import Functional.Prelude
import GHC.Conc (setUncaughtExceptionHandler)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.OsPath (encodeUtf)
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))

-- | Runs functional tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  Tasty.defaultMain
    $ Tasty.localOption OnPass
    $ Tasty.withResource setup teardown
    $ \env ->
      let setBackend b = set' #backend b <$> env

          tests =
            [ Delete.tests,
              PermDelete.tests,
              Empty.tests,
              Restore.tests,
              List.tests,
              Metadata.tests,
              Convert.tests,
              Merge.tests
            ]

          perBackendTests =
            backendDescs <&> \(backend, desc) ->
              testGroup
                desc
                (fmap ($ setBackend backend) tests)
       in testGroup "Functional Tests" perBackendTests
  where
    backendDescs :: [(Backend, String)]
    backendDescs =
      [minBound .. maxBound] <&> \b ->
        (b, titleCase $ Backend.backendName b)

    titleCase [] = []
    titleCase (c : cs) = Ch.toTitle c : cs

setup :: IO TestEnv
setup = do
  tmpDir <- (\tmp -> tmp </> pathCharon </> [osp|functional|]) <$> Dir.getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  testDir <- encodeUtf ""
  pure
    $ MkTestEnv
      { backend = BackendCbor,
        testDir,
        trashDir = pathDotTrash,
        testRoot = tmpDir
      }

teardown :: TestEnv -> IO ()
teardown env = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    fp = env ^. #testRoot
    cleanup = removePathForcibly fp
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> show fp
