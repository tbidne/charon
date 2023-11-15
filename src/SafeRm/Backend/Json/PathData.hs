{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the Json backend.
module SafeRm.Backend.Json.PathData
  ( -- * PathData
    PathData (..),
    toPathData,
    toCorePathData,
    fromCorePathData,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    (.:),
  )
import Data.Aeson qualified as Asn
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

newtype PathDataJSON = MkPathDataJSON (FilePath, Timestamp)

instance ToJSON PathDataJSON where
  toJSON (MkPathDataJSON (opathStr, ts)) =
    Asn.object
      [ "path" .= opathStr,
        "created" .= ts
      ]

instance FromJSON PathDataJSON where
  parseJSON = Asn.withObject "PathDataJSON" $ \v ->
    fmap MkPathDataJSON
      $ (,)
      <$> v
      .: "path"
      <*> v
      .: "created"

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> Either String ByteString
  encode (UnsafePathData _ (MkPathI opath) ts) =
    bimap
      displayException
      (BSL.toStrict . Asn.encode . MkPathDataJSON . (,ts))
      (decodeOsToFp opath)

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    MkPathDataJSON (opathStr, ts) <- Asn.eitherDecode $ BSL.fromStrict bs

    bimap
      displayException
      (\opath -> UnsafePathData name (MkPathI opath) ts)
      (encodeFpToOs opathStr)

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
