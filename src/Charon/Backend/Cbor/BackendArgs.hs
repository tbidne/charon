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
        toCorePathData,
        toPd
      ),
  )
import Charon.Prelude

backendArgs ::
  ( MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  BackendArgs m Cbor.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendCbor,
      toPd = Cbor.PathData.toPathData,
      toCorePathData = const (pure . Cbor.PathData.toCorePathData),
      fromCorePathData = Cbor.PathData.fromCorePathData
    }