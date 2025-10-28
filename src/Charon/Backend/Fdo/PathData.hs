{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides 'PathData' for use with the FreeDesktopOrg backend.
module Charon.Backend.Fdo.PathData
  ( -- * PathData
    PathData (..),
    toPathData,
    toCorePathData,
    toCorePathDataDirectorySizes,
    fromCorePathData,
  )
where

import Charon.Backend.Default.Trash qualified as Trash
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Backend.Fdo.DirectorySizes (DirectorySizesEntry)
import Charon.Backend.Fdo.Utils qualified as Fdo.Utils
import Charon.Class.Serial (Serial (..), decodeUnit)
import Charon.Data.PathData qualified as PathData
import Charon.Data.PathType (PathTypeW)
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashHome
      ),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.Timestamp (Timestamp)
import Charon.Prelude
import Charon.Utils qualified as U
import Charon.Utils qualified as Utils
import Data.ByteString.Char8 qualified as C8
import Data.HashMap.Strict qualified as HMap
import Data.HashSet qualified as Set
import Effects.FileSystem.PathReader qualified as PR
import Numeric.Algebra (ASemigroup ((.+.)))

-- | Data for an Fdo path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The path to be used in the trash directory.
    fileName :: PathI TrashEntryFileName,
    -- | The original path on the file system.
    originalPath :: PathI TrashEntryOriginalPath,
    -- | Time this entry was created.
    created :: Timestamp
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

makeFieldLabelsNoPrefix ''PathData

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
toPathData ::
  ( HasCallStack,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathTypeW)
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
  (fileName', originalPath', pathType) <- Default.Utils.getPathInfo trashHome origPath

  pure
    ( UnsafePathData
        { fileName = fileName',
          originalPath = originalPath',
          created = currTime
        },
      pathType
    )

instance Serial PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> Either String ByteString
  encode pd = do
    opath <- encode (pd ^. #originalPath)
    created <- encode (pd ^. #created)
    opath' <- U.percentEncode opath
    Right
      $ C8.unlines
        [ "[Trash Info]",
          "Path=" <> opath',
          "DeletionDate=" <> created
        ]

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    mp <- Default.Utils.parseTrashInfoMap expectedKeys bs

    originalPath <- decodeUnit =<< U.percentDecode =<< Default.Utils.lookup "Path" mp
    created <- decodeUnit =<< Default.Utils.lookup "DeletionDate" mp

    Right
      $ UnsafePathData
        { fileName = name,
          originalPath,
          created
        }
    where
      expectedKeys = Set.fromList ["Path", "DeletionDate"]

toCorePathData ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  PathData ->
  m PathData.PathData
toCorePathData trashHome pd = addNamespace "toCorePathData" $ do
  $(logDebug) $ "PathData: " <> showt pd
  pathType <- Default.Utils.pathDataToType trashHome pd

  size <- Utils.getPathSize path

  pure
    $ PathData.UnsafePathData
      { pathType,
        fileName = pd ^. #fileName,
        originalPath = pd ^. #originalPath,
        size,
        created = pd ^. #created
      }
  where
    MkPathI path = Trash.getTrashPath trashHome (pd ^. #fileName)

toCorePathDataDirectorySizes ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixFilesC m,
    MonadTerminal m
  ) =>
  HashMap ByteString DirectorySizesEntry ->
  PathI TrashHome ->
  PathData ->
  m PathData.PathData
toCorePathDataDirectorySizes dsizeMap trashHome pd = addNamespace "toCorePathDataDirectorySizes" $ do
  $(logDebug) $ "PathData: " <> showt pd
  pathType <- Default.Utils.pathDataToType trashHome pd

  size <- case pathType ^. #unPathTypeW of
    PathTypeFile -> fromℤ <$> PR.getFileSize path
    PathTypeOther -> fromℤ <$> PR.getFileSize path
    PathTypeDirectory -> do
      name <- Fdo.Utils.percentEncodeFileName pd
      case HMap.lookup name dsizeMap of
        Just entry -> do
          dirSize <- fromIntegral @_ @Natural <$> PR.getFileSize path
          -- directorysizes does not include the directory itself, so we have
          -- to add it back
          pure $ MkBytes dirSize .+. entry ^. #size
        Nothing -> do
          $(logWarn)
            $ "Directory not found in directorysizes, calculating directly: "
            <> Paths.toText (pd ^. #fileName)
          Utils.getPathSize path
    PathTypeSymbolicLink -> Utils.getSymLinkSize path

  pure
    $ PathData.UnsafePathData
      { pathType,
        fileName = pd ^. #fileName,
        originalPath = pd ^. #originalPath,
        size,
        created = pd ^. #created
      }
  where
    MkPathI path = Trash.getTrashPath trashHome (pd ^. #fileName)

fromCorePathData ::
  PathData.PathData ->
  PathData
fromCorePathData pd =
  UnsafePathData
    { fileName = pd ^. #fileName,
      originalPath = pd ^. #originalPath,
      created = pd ^. #created
    }
