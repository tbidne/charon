module Charon.Backend.Cbor.BackendArgs
  ( backendArgs,
  )
where

import Charon.Backend.Cbor.PathData qualified as Cbor.PathData
import Charon.Backend.Data (Backend (BackendCbor))
import Charon.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        makePathData,
        toCorePathData
      ),
  )
import Charon.Prelude

backendArgs ::
  ( MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixC m,
    MonadTerminal m
  ) =>
  BackendArgs m Cbor.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendCbor,
      makePathData = Cbor.PathData.toPathData,
      toCorePathData = const (pure . Cbor.PathData.toCorePathData),
      fromCorePathData = Cbor.PathData.fromCorePathData
    }
