{-# LANGUAGE QuasiQuotes #-}

-- | Provides classes for running Charon with an environment.
module Charon.Env
  ( HasTrashHome (..),
    getTrashLog,
    HasBackend (..),
  )
where

import Charon.Backend.Data (Backend)
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashHome,
        TrashLog
      ),
  )
import Charon.Prelude
import Effects.FileSystem.PathReader (getXdgState)

-- | Class for retrieving the trash home.
class HasTrashHome a where
  -- | Retrieves the trash home path.
  getTrashHome :: a -> PathI TrashHome
  default getTrashHome ::
    ( Is k A_Getter,
      LabelOptic' "trashHome" k a (PathI TrashHome)
    ) =>
    a ->
    PathI TrashHome
  getTrashHome = view #trashHome

-- | Retrieves the trash log path.
getTrashLog :: (HasCallStack, MonadPathReader m) => m (PathI TrashLog)
getTrashLog = MkPathI . (</> [osp|charon.log|]) <$> getXdgState pathCharon

-- | Class for retrieving the backend.
class HasBackend a where
  -- | Retrieves the trash home path.
  getBackend :: a -> Backend
  default getBackend ::
    ( Is k A_Getter,
      LabelOptic' "backend" k a Backend
    ) =>
    a ->
    Backend
  getBackend = view #backend
