{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Backend.Default.BackendArgs
  ( BackendArgs (..),
  )
where

import Charon.Backend.Data (Backend)
import Charon.Data.PathData (PathData)
import Charon.Data.PathType (PathTypeW)
import Charon.Data.Paths (PathI, PathIndex (TrashEntryOriginalPath, TrashHome))
import Charon.Data.Timestamp (Timestamp)
import Charon.Prelude

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
