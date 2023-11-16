module SafeRm.Backend.Cbor.BackendArgs
  ( backendArgs,
  )
where

import SafeRm.Backend.Cbor.PathData qualified as Cbor.PathData
import SafeRm.Backend.Data (Backend (BackendCbor))
import SafeRm.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        toCorePathData,
        toPd
      ),
  )
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
  BackendArgs m Cbor.PathData.PathData
backendArgs =
  MkBackendArgs
    { backend = BackendCbor,
      toPd = Cbor.PathData.toPathData,
      toCorePathData = Cbor.PathData.toCorePathData,
      fromCorePathData = Cbor.PathData.fromCorePathData
    }