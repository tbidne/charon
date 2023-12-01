{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeRm.Backend.Default.BackendArgs
  ( BackendArgs (..),
  )
where

import SafeRm.Backend.Data (Backend)
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathType (PathTypeW)
import SafeRm.Data.Paths (PathI, PathIndex (TrashEntryOriginalPath, TrashHome))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude

data BackendArgs m pd = MkBackendArgs
  { backend :: Backend,
    toPd ::
      Timestamp ->
      PathI TrashHome ->
      PathI TrashEntryOriginalPath ->
      m (pd, PathTypeW),
    toCorePathData :: PathI TrashHome -> pd -> m PathData,
    fromCorePathData :: PathData -> pd
  }

makeFieldLabelsNoPrefix ''BackendArgs
