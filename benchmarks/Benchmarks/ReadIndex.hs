-- | Benchmarks for reading the index.
module Benchmarks.ReadIndex
  ( setup,
    benchmarks,
  )
where

import Benchmarks.Prelude
import SafeRm qualified
import SafeRm.Data.Backend (Backend (..))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.UniqueSeq qualified as UniqueSeq
import SafeRm.Runner.Env (Env (..), LogEnv (MkLogEnv))
import SafeRm.Runner.SafeRmT (runSafeRmT)

-- | Index reading benchmarks.
benchmarks :: FilePath -> Benchmark
benchmarks tmpDir = do
  bgroup
    "Read Index"
    [ readIndex "1,000" (MkPathI $ tmpDir </> "read1/.trash"),
      readIndex "10,000" (MkPathI $ tmpDir </> "read2/.trash"),
      readIndex "100,000" (MkPathI $ tmpDir </> "read3/.trash")
    ]

-- | Setup for index reading.
setup :: FilePath -> IO ()
setup testDir = do
  setupRead r1 [1 .. 1_000]
  setupRead r2 [1 .. 10_000]
  setupRead r3 [1 .. 100_000]
  where
    r1 = testDir </> "read1/"
    r2 = testDir </> "read2/"
    r3 = testDir </> "read3/"

    setupRead :: FilePath -> [Int] -> IO ()
    setupRead dir files = do
      clearDirectory dir
      clearDirectory trashDir

      uniqueSeqRef <- newIORef UniqueSeq.empty

      for_ files $ \filename -> do
        let filepath = dir </> show filename
        writeBinaryFile filepath ""
        modifyIORef' uniqueSeqRef (`UniqueSeq.append` MkPathI filepath)

      uniqueSeq <- readIORef uniqueSeqRef
      env <- mkEnv $ MkPathI trashDir
      runSafeRmT (SafeRm.delete uniqueSeq) env
      where
        trashDir = dir </> ".trash/"

readIndex :: String -> PathI TrashHome -> Benchmark
readIndex desc =
  bench desc
    . nfIO
    . (runSafeRmT SafeRm.getIndex <=< mkEnv)

mkEnv :: PathI TrashHome -> IO (Env IO)
mkEnv trashHome = do
  pure $
    MkEnv
      { trashHome = trashHome,
        backend = BackendDefault,
        logEnv = MkLogEnv Nothing ""
      }
