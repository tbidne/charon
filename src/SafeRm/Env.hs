-- | Provides classes for running SafeRm with an environment.
--
-- @since 0.1
module SafeRm.Env
  ( HasTrashHome (..),
    getTrashLog,
    getTrashPathDir,
    getTrashInfoDir,
    getTrashPath,
    getTrashInfoPath,
  )
where

import SafeRm.Data.Paths
  ( PathI,
    PathIndex (..),
    liftPathI',
    (<//>),
  )
import SafeRm.Prelude

-- | Class for retrieving the trash home.
--
-- @since 0.1
class HasTrashHome a where
  -- | Retrieves the trash home path.
  --
  -- @since 0.1
  getTrashHome :: a -> PathI TrashHome
  default getTrashHome ::
    ( Is k A_Getter,
      LabelOptic' "trashHome" k a (PathI TrashHome)
    ) =>
    a ->
    PathI TrashHome
  getTrashHome = view #trashHome

-- | Retrieves the trash log path.
--
-- @since 0.1
getTrashLog :: PathI TrashHome -> PathI TrashLog
getTrashLog = (<//> "log")

-- | @since 0.1
getTrashPath :: PathI TrashHome -> PathI TrashName -> PathI TrashPath
getTrashPath trashHome name = trashHome <//> "paths" <//> name

-- | @since 0.1
getTrashInfoPath :: PathI TrashHome -> PathI TrashName -> PathI TrashInfoPath
getTrashInfoPath trashHome name = trashHome <//> "info" <//> liftPathI' (<> ".info") name

-- | Retrieves the trash path dir.
--
-- @since 0.1
getTrashPathDir :: PathI TrashHome -> PathI TrashPathDir
getTrashPathDir trashHome = trashHome <//> "paths"

-- | Retrieves the trash info dir.
--
-- @since 0.1
getTrashInfoDir :: PathI TrashHome -> PathI TrashInfoDir
getTrashInfoDir trashHome = trashHome <//> "info"
