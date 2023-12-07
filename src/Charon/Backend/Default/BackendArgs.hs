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

-- | These args are provided backends that want to use the Default
-- functionality.
data BackendArgs m pd = MkBackendArgs
  { -- | The backend type.
    backend :: Backend,
    -- | Function for retrieving the custom path data pd.
    toPd ::
      Timestamp ->
      PathI TrashHome ->
      PathI TrashEntryOriginalPath ->
      m (pd, PathTypeW),
    -- | Transformation from custom pd to common PathData.
    toCorePathData :: PathI TrashHome -> pd -> m PathData,
    -- | Transformation from common PathData to custom pd.
    fromCorePathData :: PathData -> pd
  }

makeFieldLabelsNoPrefix ''BackendArgs
