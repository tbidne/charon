{-# LANGUAGE QuasiQuotes #-}

-- | Benchmarks for reading the index.
module Benchmarks.ReadIndex
  ( setup,
    benchmarks,
  )
where

import Benchmarks.Prelude
import Charon qualified
import Charon.Backend.Data (Backend (BackendCbor))
import Charon.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import Charon.Data.UniqueSeq qualified as USeq
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Runner.CharonT (runCharonT)
import Charon.Runner.Command.Delete
  ( DeleteParams (MkDeleteParams, paths, prompt, verbose),
  )
import Charon.Runner.Config
  ( CoreConfig (MkCoreConfig, backend, logging, trashHome),
    LogEnv (MkLogEnv),
  )
import Charon.Runner.Env (Env (MkEnv, coreConfig))
import Charon.Runner.Phase (Prompt (MkPrompt), Verbose (MkVerbose))
import Effects.Haskeline qualified as EH
import FileSystem.OsPath ((</>!))

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

      uniqueSeqRef <- newIORef USeq.empty

      for_ files $ \filename -> do
        let filepath = dir </>! show filename
        writeBinaryFile filepath ""
        modifyIORef' uniqueSeqRef (`USeq.append` MkPathI filepath)

      uniqueSeq <- readIORef uniqueSeqRef
      let params =
            MkDeleteParams
              { paths = USeqNE.unsafefromUniqueSeq uniqueSeq,
                prompt = MkPrompt False,
                verbose = MkVerbose False
              }

      env <- mkEnv $ MkPathI trashDir
      EH.runInputTEnv $ runReaderT (runCharonT (Charon.delete params) env)
      where
        trashDir = dir </> [osp|.trash/|]

readIndex :: String -> PathI TrashHome -> Benchmark
readIndex desc =
  bench desc
    . nfIO
    . (runCharonT Charon.getIndex <=< mkEnv)

mkEnv :: PathI TrashHome -> IO Env
mkEnv trashHome = do
  pure
    $ MkEnv
      { coreConfig =
          MkCoreConfig
            { trashHome = trashHome,
              backend = BackendCbor,
              logging = MkLogEnv Nothing ""
            }
      }
