module Charon.Backend.Fdo.BackendArgs
  ( backendArgs,
    backendArgsDirectorySizes,
  )
where

import Charon.Backend.Data (Backend (BackendFdo))
import Charon.Backend.Default.BackendArgs
  ( BackendArgs
      ( MkBackendArgs,
        backend,
        fromCorePathData,
        makePathData,
        toCorePathData
      ),
  )
import Charon.Backend.Fdo.DirectorySizes (DirectorySizesEntry)
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
      makePathData = Fdo.PathData.toPathData,
      toCorePathData = Fdo.PathData.toCorePathData,
      fromCorePathData = Fdo.PathData.fromCorePathData
    }

-- | Like backendArgs, but uses directorysizes to transform
-- Fdo PathData -> Core PathData instead of calculating size on the fly.
-- This has the potential to be faster when we expect to do many calculations
-- (e.g. reading the index).
backendArgsDirectorySizes ::
  ( MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  HashMap ByteString DirectorySizesEntry ->
  BackendArgs m Fdo.PathData.PathData
backendArgsDirectorySizes dsizeMap =
  MkBackendArgs
    { backend = BackendFdo,
      makePathData = Fdo.PathData.toPathData,
      toCorePathData = Fdo.PathData.toCorePathDataDirectorySizes dsizeMap,
      fromCorePathData = Fdo.PathData.fromCorePathData
    }
