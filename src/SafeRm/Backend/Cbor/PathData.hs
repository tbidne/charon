{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the Cbor backend.
module SafeRm.Backend.Cbor.PathData
  ( -- * PathData
    PathData (..),
    toPathData,
    toCorePathData,
    fromCorePathData,
  )
where

import Codec.Serialise qualified as Serialise
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy qualified as BSL
import SafeRm.Backend.Default.Trash qualified as Trash
import SafeRm.Backend.Default.Utils qualified as Default.Utils
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashHome
      ),
  )
import SafeRm.Data.Serialize (Serialize (DecodeExtra, decode, encode))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils

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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathType)
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

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> Either String ByteString
  encode (UnsafePathData _ (MkPathI opath) ts) =
    case decodeOsToFp opath of
      Right opath' -> pure $ BSL.toStrict $ Serialise.serialise (opath', ts)
      Left ex -> Left $ displayException ex

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    (opath, created) <- first show $ Serialise.deserialiseOrFail (BSL.fromStrict bs)

    case encodeFpToOs opath of
      Right opath' ->
        Right
          $ UnsafePathData
            { fileName = name,
              originalPath = MkPathI opath',
              created
            }
      Left ex -> Left $ displayException ex

toCorePathData ::
  ( HasCallStack,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadThread m
  ) =>
  PathI TrashHome ->
  PathData ->
  m PathData.PathData
toCorePathData trashHome pd = do
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

fromCorePathData ::
  ( Monad m
  ) =>
  PathData.PathData ->
  m PathData
fromCorePathData pd =
  pure
    $ UnsafePathData
      { fileName = pd ^. #fileName,
        originalPath = pd ^. #originalPath,
        created = pd ^. #created
      }
