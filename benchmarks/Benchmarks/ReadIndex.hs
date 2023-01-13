-- | Benchmarks for reading the index.
--
-- @since 0.1
module Benchmarks.ReadIndex
  ( setup,
    benchmarks,
  )
where

import Benchmarks.Prelude
import Data.ByteString.Char8 qualified as Char8
import SafeRm qualified
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Runner.Env (Env (MkEnv, logEnv, trashHome), LogEnv (MkLogEnv))
import SafeRm.Runner.SafeRmT

-- | Index reading benchmarks.
--
-- @since 0.1
benchmarks :: FilePath -> Benchmark
benchmarks tmpDir = do
  bgroup
    "Read Index"
    [ readIndex "1,000" (MkPathI $ tmpDir </> "read1/.trash"),
      readIndex "10,000" (MkPathI $ tmpDir </> "read2/.trash"),
      readIndex "100,000" (MkPathI $ tmpDir </> "read3/.trash"),
      readIndex "1,000,000" (MkPathI $ tmpDir </> "read4/.trash")
    ]

-- | Setup for index reading.
--
-- @since 0.1
setup :: FilePath -> IO ()
setup testDir = do
  setupRead r1 [1 .. 1_000]
  setupRead r2 [1 .. 10_000]
  setupRead r3 [1 .. 100_000]
  setupRead r4 [1 .. 1_000_000]
  where
    r1 = testDir </> "read1/"
    r2 = testDir </> "read2/"
    r3 = testDir </> "read3/"
    r4 = testDir </> "read4/"

    setupRead :: FilePath -> [Int] -> IO ()
    setupRead dir files = do
      clearDirectory dir
      clearDirectory trashDir
      appendBinaryFile indexPath header

      for_ files $ \filename -> do
        let filepath = trashDir </> show filename
        clearDirectory trashDir
        writeBinaryFile filepath ""
        appendBinaryFile indexPath (Char8.pack $ mkEntry dir filename)
      where
        trashDir = dir </> ".trash/"
        indexPath = trashDir </> ".index.csv"
        mkEntry d' f =
          mconcat
            [ "file,",
              show f,
              ",",
              d' </> show f,
              ",2022-09-28 12:00:00\n"
            ]

readIndex :: String -> PathI TrashHome -> Benchmark
readIndex desc =
  bench desc
    . nfIO
    . (runSafeRmT SafeRm.getIndex <=< mkEnv)

mkEnv :: PathI TrashHome -> IO Env
mkEnv trashHome = do
  pure $
    MkEnv
      { trashHome = trashHome,
        logEnv = MkLogEnv Nothing ""
      }
