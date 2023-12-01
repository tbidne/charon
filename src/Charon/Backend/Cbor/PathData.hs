{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the Cbor backend.
module Charon.Backend.Cbor.PathData
  ( -- * PathData
    PathData (..),
    toPathData,
    toCorePathData,
    fromCorePathData,
  )
where

import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Class.Serial (Serial (DecodeExtra, decode, encode))
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
import Charon.Data.Timestamp (Timestamp)
import Charon.Prelude
import Charon.Utils qualified as Utils
import Codec.Serialise qualified as Serialise
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy qualified as BSL

-- | Data for an Fdo path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The path type.
    pathType :: PathTypeW,
    -- | The path to be used in the trash directory.
    fileName :: PathI TrashEntryFileName,
    -- | The original path on the file system.
    originalPath :: PathI TrashEntryOriginalPath,
    -- | Time this entry was created.
    created :: Timestamp,
    -- | The size.
    size :: Bytes B Natural
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
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathTypeW)
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
  (fileName, originalPath, pathType) <- Default.Utils.getPathInfo trashHome origPath
  size <- Utils.getPathSize (originalPath ^. #unPathI)

  pure
    ( UnsafePathData
        { pathType,
          fileName,
          originalPath,
          created = currTime,
          size
        },
      pathType
    )

instance Serial PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> Either String ByteString
  encode (UnsafePathData pathType _ (MkPathI opath) ts (MkBytes sz)) =
    case decodeOsToFp opath of
      Right opath' -> pure $ BSL.toStrict $ Serialise.serialise (pathType, opath', ts, sz)
      Left ex -> Left $ displayException ex

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    (pathType, opath, created, size) <- first show $ Serialise.deserialiseOrFail (BSL.fromStrict bs)

    case encodeFpToOs opath of
      Right opath' ->
        Right
          $ UnsafePathData
            { pathType,
              fileName = name,
              originalPath = MkPathI opath',
              created,
              size = MkBytes size
            }
      Left ex -> Left $ displayException ex

toCorePathData :: PathData -> PathData.PathData
toCorePathData pd = do
  PathData.UnsafePathData
    { pathType = pd ^. #pathType,
      fileName = pd ^. #fileName,
      originalPath = pd ^. #originalPath,
      size = pd ^. #size,
      created = pd ^. #created
    }

fromCorePathData :: PathData.PathData -> PathData
fromCorePathData pd =
  UnsafePathData
    { pathType = pd ^. #pathType,
      fileName = pd ^. #fileName,
      originalPath = pd ^. #originalPath,
      created = pd ^. #created,
      size = pd ^. #size
    }
