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
import SafeRm.Backend.Default.Utils qualified as Default.Utils
import SafeRm.Class.Serial (Serial (DecodeExtra, decode, encode))
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
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils

-- | Data for an Fdo path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The path type.
    pathType :: PathType,
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
  m (PathData, PathType)
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
  (fileName', originalPath', pathType) <- Default.Utils.getPathInfo trashHome origPath
  size <- Utils.getPathSize (origPath ^. #unPathI)

  pure
    ( UnsafePathData
        { pathType,
          fileName = fileName',
          originalPath = originalPath',
          created = currTime,
          size
        },
      pathType
    )

newtype PathDataJSON = MkPathDataJSON (PathType, FilePath, Timestamp, Natural)

instance ToJSON PathDataJSON where
  toJSON (MkPathDataJSON (pathType, opathStr, ts, size)) =
    Asn.object
      [ "pathType" .= pathType,
        "path" .= opathStr,
        "created" .= ts,
        "size" .= size
      ]

instance FromJSON PathDataJSON where
  parseJSON = Asn.withObject "PathDataJSON" $ \v ->
    fmap MkPathDataJSON
      $ (,,,)
      <$> v
      .: "pathType"
      <*> v
      .: "path"
      <*> v
      .: "created"
      <*> v
      .: "size"

instance Serial PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> Either String ByteString
  encode (UnsafePathData pathType _ (MkPathI opath) ts (MkBytes sz)) =
    bimap
      displayException
      (BSL.toStrict . Asn.encode . MkPathDataJSON . (pathType,,ts,sz))
      (decodeOsToFp opath)

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    MkPathDataJSON (pathType, opathStr, ts, sz) <- Asn.eitherDecode $ BSL.fromStrict bs

    bimap
      displayException
      (\opath -> UnsafePathData pathType name (MkPathI opath) ts (MkBytes sz))
      (encodeFpToOs opathStr)

toCorePathData :: PathData -> PathData.PathData
toCorePathData pd =
  PathData.UnsafePathData
    { pathType = pd ^. #pathType,
      fileName = pd ^. #fileName,
      originalPath = pd ^. #originalPath,
      created = pd ^. #created,
      size = pd ^. #size
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
