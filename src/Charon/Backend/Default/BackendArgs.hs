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

-- We end up manually threading backendArgs through many of our Default/Trash
-- functions. It is tempting to create a reader env for this, and then change
-- our signatures to HasBackendArgs. Unfortunately the pd type param gets in
-- our way.
--
-- This is, our signatures end up looking like
--
-- foo :: forall m env pd. (HasBackendArgs env m pd, MonadReader env m)
--
-- but GHC cannot infer that we are passing the same type pd throughout the
-- program, so we have to add manual annotations e.g.
--
-- foo @_ @_ @pd
--
-- Thus we are back to manual threading, defeating the purpose of using
-- Reader.

-- | Additional params used by the default backend that are parameterized on
-- the backend-specific PathData, pd.
data BackendArgs m pd = MkBackendArgs
  { -- | The backend type.
    backend :: Backend,
    -- | Function for creating the custom path data pd.
    makePathData ::
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
