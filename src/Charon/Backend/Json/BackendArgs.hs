module Charon.Backend.Json.BackendArgs
  ( backendArgs,
  )
where

import Charon.Backend.Data (Backend (BackendJson))
import Charon.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        toCorePathData,
        toPd
      ),
  )
import Charon.Backend.Json.PathData qualified as Json.PathData
import Charon.Prelude

backendArgs ::
  ( MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  BackendArgs m Json.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendJson,
      toPd = Json.PathData.toPathData,
      toCorePathData = const (pure . Json.PathData.toCorePathData),
      fromCorePathData = Json.PathData.fromCorePathData
    }
