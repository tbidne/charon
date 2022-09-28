-- | Benchmark suite.
--
-- @since 0.1
module Main (main) where

import Benchmarks.Prelude
import Benchmarks.ReadIndex qualified as ReadIndex
import System.Directory qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')

main :: IO ()
main = bracket setup runBenchmarks teardown
  where
    runBenchmarks testDir =
      defaultMain
        [ ReadIndex.benchmarks testDir
        ]

setup :: IO FilePath
setup = do
  testDir <- (</> "bench") <$> Dir.getTemporaryDirectory
  Dir.createDirectoryIfMissing False testDir
  ReadIndex.setup testDir
  pure testDir

teardown :: FilePath -> IO ()
teardown testDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly testDir
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> testDir
