module SafeRm.Backend.Fdo.BackendArgs
  ( backendArgs,
  )
where

import SafeRm.Backend.Data (Backend (BackendFdo))
import SafeRm.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        toCorePathData,
        toPd
      ),
  )
import SafeRm.Backend.Fdo.PathData qualified as Fdo.PathData
import SafeRm.Prelude

backendArgs ::
  ( MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  BackendArgs m Fdo.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendFdo,
      toPd = Fdo.PathData.toPathData,
      toCorePathData = Fdo.PathData.toCorePathData,
      fromCorePathData = Fdo.PathData.fromCorePathData
    }
