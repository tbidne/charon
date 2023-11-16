module SafeRm.Backend.Json.BackendArgs
  ( backendArgs,
  )
where

import SafeRm.Backend.Data (Backend (BackendJson))
import SafeRm.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        toCorePathData,
        toPd
      ),
  )
import SafeRm.Backend.Json.PathData qualified as Json.PathData
import SafeRm.Prelude

backendArgs ::
  ( MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadThread m
  ) =>
  BackendArgs m Json.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendJson,
      toPd = Json.PathData.toPathData,
      toCorePathData = Json.PathData.toCorePathData,
      fromCorePathData = Json.PathData.fromCorePathData
    }