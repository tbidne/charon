{-# LANGUAGE QuasiQuotes #-}

-- | Benchmarks for reading the index.
module Benchmarks.ReadIndex
  ( setup,
    benchmarks,
  )
where

import Benchmarks.Prelude
import Effectful.FileSystem.FileReader.Dynamic (runFileReaderDynamicIO)
import Effectful.FileSystem.FileWriter.Dynamic (runFileWriterDynamicIO)
import Effectful.FileSystem.FileWriter.Static (FileWriterStatic)
import Effectful.FileSystem.FileWriter.Static qualified as FWStatic
import Effectful.FileSystem.HandleWriter.Dynamic (runHandleWriterDynamicIO)
import Effectful.FileSystem.Utils ((</>!))
import Effectful.Terminal.Dynamic (runTerminalDynamicIO)
import Effectful.Time.Dynamic (runTimeDynamicIO)
import SafeRm qualified
import SafeRm.Data.Backend (Backend (BackendCbor))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (TrashHome))
import SafeRm.Data.UniqueSeq qualified as UniqueSeq
import SafeRm.Runner.Env
  ( Env (MkEnv, backend, logEnv, trashHome),
    LogEnv (MkLogEnv),
  )
import SafeRm.Runner.Logging (runLoggerDynamic, runLoggerNSDynamic)

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
setup ::
  forall es.
  ( FileWriterStatic :> es,
    IOE :> es,
    IORefStatic :> es
  ) =>
  OsPath ->
  Eff es ()
setup testDir = do
  setupRead r1 [1 .. 1_000]
  setupRead r2 [1 .. 10_000]
  setupRead r3 [1 .. 100_000]
  where
    r1 = testDir </> [osp|read1/|]
    r2 = testDir </> [osp|read2/|]
    r3 = testDir </> [osp|read3/|]

    setupRead :: OsPath -> [Int] -> Eff es ()
    setupRead dir files = do
      clearDirectory dir
      clearDirectory trashDir

      uniqueSeqRef <- newIORef UniqueSeq.empty

      for_ files $ \filename -> do
        let filepath = dir </>! show filename
        FWStatic.writeBinaryFile filepath ""
        modifyIORef' uniqueSeqRef (`UniqueSeq.append` MkPathI filepath)

      uniqueSeq <- readIORef uniqueSeqRef
      let env = mkEnv $ MkPathI trashDir
      runSafeRm env (SafeRm.delete @Env uniqueSeq)
      where
        trashDir = dir </> [osp|.trash/|]

readIndex :: String -> PathI TrashHome -> Benchmark
readIndex desc thome =
  bench desc
    $ nfIO
    $ runEff (runSafeRm (mkEnv thome) (SafeRm.getIndex @Env))

mkEnv :: PathI TrashHome -> Env
mkEnv trashHome =
  MkEnv
    { trashHome = trashHome,
      backend = BackendCbor,
      logEnv = MkLogEnv Nothing ""
    }

runSafeRm ::
  (IOE :> es) =>
  Env ->
  Eff
    ( LoggerDynamic
        : LoggerNSDynamic
        : FileWriterDynamic
        : PathWriterDynamic
        : PathReaderDynamic
        : TerminalDynamic
        : TimeDynamic
        : HandleWriterDynamic
        : FileReaderDynamic
        : IORefStatic
        : Reader Env
        : es
    )
    a ->
  Eff es a
runSafeRm env =
  runReader env
    . runIORefStaticIO
    . runFileReaderDynamicIO
    . runHandleWriterDynamicIO
    . runTimeDynamicIO
    . runTerminalDynamicIO
    . runPathReaderDynamicIO
    . runPathWriterDynamicIO
    . runFileWriterDynamicIO
    . runLoggerNSDynamic
    . runLoggerDynamic
