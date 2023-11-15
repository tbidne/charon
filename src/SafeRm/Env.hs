{-# LANGUAGE QuasiQuotes #-}

-- | Provides classes for running SafeRm with an environment.
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashLog,
    HasBackend (..),
  )
where

import Effects.FileSystem.PathReader (getXdgState)
import SafeRm.Backend.Data (Backend)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashHome,
        TrashLog
      ),
  )
import SafeRm.Prelude

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
getTrashLog = MkPathI . (</> [osp|log|]) <$> getXdgState pathSafeRm

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
