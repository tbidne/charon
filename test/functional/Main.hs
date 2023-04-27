-- | Entrypoint for functional tests.
module Main (main) where

import Effects.FileSystem.PathReader qualified as Dir
import Functional.Commands.Delete qualified as Delete
import Functional.Commands.Empty qualified as Empty
import Functional.Commands.List qualified as List
import Functional.Commands.Metadata qualified as Metadata
import Functional.Commands.PermDelete qualified as PermDelete
import Functional.Commands.Restore qualified as Restore
import Functional.Prelude
import GHC.Conc (setUncaughtExceptionHandler)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty

-- | Runs functional tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  Tasty.defaultMain $
    Tasty.withResource setup teardown $ \args ->
      testGroup
        "Functional Tests"
        [ Delete.tests args,
          PermDelete.tests args,
          Empty.tests args,
          Restore.tests args,
          List.tests args,
          Metadata.tests args
        ]

setup :: IO FilePath
setup = do
  tmpDir <- (\tmp -> tmp </> "safe-rm" </> "functional") <$> Dir.getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: FilePath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly fp
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> fp
