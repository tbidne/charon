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
        makePathData,
        toCorePathData
      ),
  )
import Charon.Backend.Json.PathData qualified as Json.PathData
import Charon.Prelude

backendArgs ::
  ( MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixC m,
    MonadTerminal m
  ) =>
  BackendArgs m Json.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendJson,
      makePathData = Json.PathData.toPathData,
      toCorePathData = const (pure . Json.PathData.toCorePathData),
      fromCorePathData = Json.PathData.fromCorePathData
    }
