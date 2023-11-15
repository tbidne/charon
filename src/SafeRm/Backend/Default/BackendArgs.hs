{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeRm.Backend.Default.BackendArgs
  ( BackendArgs (..),
  )
where

import SafeRm.Backend.Data (Backend)
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.Paths (PathI, PathIndex (TrashEntryOriginalPath, TrashHome))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude

data BackendArgs m pd = MkBackendArgs
  { backend :: Backend,
    toPd ::
      Timestamp ->
      PathI TrashHome ->
      PathI TrashEntryOriginalPath ->
      m (pd, PathType),
    toCorePathData :: PathI TrashHome -> pd -> m PathData,
    fromCorePathData :: PathData -> m pd
  }

makeFieldLabelsNoPrefix ''BackendArgs
