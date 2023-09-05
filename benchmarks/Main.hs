{-# LANGUAGE QuasiQuotes #-}

-- | Benchmark suite.
module Main (main) where

import Benchmarks.Prelude
import Benchmarks.ReadIndex qualified as ReadIndex
import Effectful.FileSystem.FileWriter.Static (FileWriterStatic)
import Effectful.FileSystem.FileWriter.Static qualified as FWStatic
import Effectful.FileSystem.PathReader.Static (PathReaderStatic)
import Effectful.FileSystem.PathReader.Static qualified as PRStatic
import Effectful.FileSystem.PathWriter.Static (PathWriterStatic)
import Effectful.FileSystem.PathWriter.Static qualified as PWStatic
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.IO qualified as IO

main :: IO ()
main = do
  setUncaughtExceptionHandler (IO.putStrLn . displayException)
  bracket setup runBenchmarks teardown
  where
    runBenchmarks testDir =
      defaultMain
        [ ReadIndex.benchmarks testDir
        ]

setup :: IO OsPath
setup = runSetup $ do
  testDir <- (</> [osp|bench|]) <$> PRStatic.getTemporaryDirectory
  PWStatic.createDirectoryIfMissing False testDir
  ReadIndex.setup testDir
  pure testDir

teardown :: OsPath -> IO ()
teardown testDir = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = runSetup $ PWStatic.removePathForcibly testDir
    doNothing =
      IO.putStrLn $ "*** Not cleaning up tmp dir: " <> show testDir

runSetup ::
  Eff
    [ PathReaderStatic,
      PathWriterStatic,
      FileWriterStatic,
      IORefStatic,
      IOE
    ]
    a ->
  IO a
runSetup =
  runEff
    . runIORefStaticIO
    . FWStatic.runFileWriterStaticIO
    . PWStatic.runPathWriterStaticIO
    . PRStatic.runPathReaderStaticIO
