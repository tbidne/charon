{-# LANGUAGE QuasiQuotes #-}

-- | Provides classes for running SafeRm with an environment.
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashLog,
    getTrashPathDir,
    getTrashInfoDir,
    getTrashPath,
    getTrashInfoPath,
    trashInfoExtension,
    trashInfoExtensionOsPath,
    HasBackend (..),
  )
where

import SafeRm.Data.Backend (Backend (BackendCbor, BackendFdo))
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashDirFiles,
        TrashDirInfo,
        TrashEntryFileName,
        TrashEntryInfo,
        TrashEntryPath,
        TrashHome,
        TrashLog
      ),
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
getTrashLog :: (PathReaderDynamic :> es) => Eff es (PathI TrashLog)
getTrashLog = MkPathI . (</> [osp|log|]) <$> getXdgState pathSafeRm

getTrashPath :: PathI TrashHome -> PathI TrashEntryFileName -> PathI TrashEntryPath
getTrashPath trashHome name = trashHome <//> MkPathI pathFiles <//> name

getTrashInfoPath ::
  Backend ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  PathI TrashEntryInfo
getTrashInfoPath backend trashHome name =
  trashHome
    <//> MkPathI pathInfo
    <//> liftPathI' (<> trashInfoExtensionOsPath backend) name

-- | Retrieves the trash path dir.
getTrashPathDir :: PathI TrashHome -> PathI TrashDirFiles
getTrashPathDir trashHome = trashHome <//> MkPathI pathFiles

-- | Retrieves the trash info dir.
getTrashInfoDir :: PathI TrashHome -> PathI TrashDirInfo
getTrashInfoDir trashHome = trashHome <//> MkPathI pathInfo

trashInfoExtensionOsPath :: Backend -> OsPath
trashInfoExtensionOsPath BackendCbor = [osp|.cbor|]
trashInfoExtensionOsPath BackendFdo = [osp|.trashinfo|]

-- | Returns the extension for the trash info files.
trashInfoExtension :: (IsString a) => Backend -> a
trashInfoExtension BackendCbor = ".cbor"
trashInfoExtension BackendFdo = ".trashinfo"

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
