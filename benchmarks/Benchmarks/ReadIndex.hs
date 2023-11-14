{-# LANGUAGE QuasiQuotes #-}

-- | Benchmarks for reading the index.
module Benchmarks.ReadIndex
  ( setup,
    benchmarks,
  )
where

import Benchmarks.Prelude
import Effects.FileSystem.Utils ((</>!))
import SafeRm qualified
import SafeRm.Backend (Backend (BackendCbor))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.UniqueSeq qualified as UniqueSeq
import SafeRm.Runner.Env
  ( Env (MkEnv, backend, logEnv, trashHome),
    LogEnv (MkLogEnv),
  )
import SafeRm.Runner.SafeRmT (runSafeRmT)

-- | Index reading benchmarks.
benchmarks :: OsPath -> Benchmark
benchmarks tmpDir = do
  bgroup
    "Read Index"
    [ readIndex "1,000" (MkPathI $ tmpDir </> [osp|read1/.trash|]),
      readIndex "10,000" (MkPathI $ tmpDir </> [osp|read2/.trash|]),
      readIndex "100,000" (MkPathI $ tmpDir </> [osp|read3/.trash|])
    ]

-- | Setup for index reading.
setup :: OsPath -> IO ()
setup testDir = do
  setupRead r1 [1 .. 1_000]
  setupRead r2 [1 .. 10_000]
  setupRead r3 [1 .. 100_000]
  where
    r1 = testDir </> [osp|read1/|]
    r2 = testDir </> [osp|read2/|]
    r3 = testDir </> [osp|read3/|]

    setupRead :: OsPath -> [Int] -> IO ()
    setupRead dir files = do
      clearDirectory dir
      clearDirectory trashDir

      uniqueSeqRef <- newIORef UniqueSeq.empty

      for_ files $ \filename -> do
        let filepath = dir </>! show filename
        writeBinaryFile filepath ""
        modifyIORef' uniqueSeqRef (`UniqueSeq.append` MkPathI filepath)

      uniqueSeq <- readIORef uniqueSeqRef
      env <- mkEnv $ MkPathI trashDir
      runSafeRmT (SafeRm.delete uniqueSeq) env
      where
        trashDir = dir </> [osp|.trash/|]

readIndex :: String -> PathI TrashHome -> Benchmark
readIndex desc =
  bench desc
    . nfIO
    . (runSafeRmT SafeRm.getIndex <=< mkEnv)

mkEnv :: PathI TrashHome -> IO (Env IO)
mkEnv trashHome = do
  pure
    $ MkEnv
      { trashHome = trashHome,
        backend = BackendCbor,
        logEnv = MkLogEnv Nothing ""
      }
