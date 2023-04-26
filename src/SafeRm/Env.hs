-- | Provides classes for running SafeRm with an environment.
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashLog,
    getTrashPathDir,
    getTrashInfoDir,
    getTrashPath,
    getTrashInfoPath,
    trashInfoExtension,
    HasBackend (..),
  )
where

import Effects.FileSystem.PathReader (getXdgState)
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (..),
    liftPathI',
    (<//>),
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
getTrashLog = MkPathI . (</> "log") <$> getXdgState "safe-rm"

getTrashPath :: PathI TrashHome -> PathI TrashEntryFileName -> PathI TrashEntryPath
getTrashPath trashHome name = trashHome <//> "files" <//> name

getTrashInfoPath :: PathI TrashHome -> PathI TrashEntryFileName -> PathI TrashEntryInfo
getTrashInfoPath trashHome name =
  trashHome
    <//> "info"
    <//> liftPathI' (<> trashInfoExtension) name

-- | Retrieves the trash path dir.
getTrashPathDir :: PathI TrashHome -> PathI TrashDirFiles
getTrashPathDir trashHome = trashHome <//> "files"

-- | Retrieves the trash info dir.
getTrashInfoDir :: PathI TrashHome -> PathI TrashDirInfo
getTrashInfoDir trashHome = trashHome <//> "info"

-- | Returns the extension for the trash info files.
trashInfoExtension :: (IsString a) => a
trashInfoExtension = ".trashinfo"

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
