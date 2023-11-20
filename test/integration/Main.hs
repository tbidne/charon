{-# LANGUAGE QuasiQuotes #-}

-- | Entrypoint for integration tests.
module Main (main) where

import Effects.FileSystem.PathReader qualified as Dir
import GHC.Conc (setUncaughtExceptionHandler)
import Integration.AsciiOnly (AsciiOnly)
import Integration.Commands.Delete qualified as Delete
import Integration.Prelude
import Integration.SafeRm qualified as SafeRm
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as T
import Test.Tasty.Options (OptionDescription (Option))

-- | Runs integration tests.
main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  T.defaultMainWithIngredients ingredients
    $ T.withResource setup teardown
    $ \args ->
      testGroup
        "Integration Tests"
        [ SafeRm.tests args,
          testGroup
            "Root Tests"
            [ Delete.tests
            ]
        ]
  where
    ingredients =
      T.includingOptions [Option @AsciiOnly Proxy] : T.defaultIngredients

setup :: IO OsPath
setup = do
  tmpDir <-
    (\p -> p </> [osp|safe-rm|] </> [osp|integration|])
      <$> Dir.getTemporaryDirectory

  clearDirectory tmpDir
  pure tmpDir

teardown :: OsPath -> IO ()
teardown tmpDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly tmpDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> show tmpDir
