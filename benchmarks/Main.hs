{-# LANGUAGE QuasiQuotes #-}

-- | Benchmark suite.
module Main (main) where

import Benchmarks.Prelude
import Benchmarks.ReadIndex qualified as ReadIndex
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter qualified as Dir
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  bracket setup runBenchmarks teardown
  where
    runBenchmarks testDir =
      defaultMain
        [ ReadIndex.benchmarks testDir
        ]

setup :: IO OsPath
setup = do
  testDir <- (</> [osp|bench|]) <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing False testDir
  ReadIndex.setup testDir
  pure testDir

teardown :: OsPath -> IO ()
teardown testDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly testDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> show testDir
