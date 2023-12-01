module Charon.Backend.Fdo.BackendArgs
  ( backendArgs,
  )
where

import Charon.Backend.Data (Backend (BackendFdo))
import Charon.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        toCorePathData,
        toPd
      ),
  )
import Charon.Backend.Fdo.PathData qualified as Fdo.PathData
import Charon.Prelude

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
