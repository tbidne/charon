{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the Json backend.
module Charon.Backend.Json.PathData
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
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    (.:),
  )
import Data.Aeson qualified as Asn
import Data.ByteString.Lazy qualified as BSL
import FileSystem.OsPath qualified as OsPath

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
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixC m,
    MonadTerminal m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathData, PathTypeW)
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

newtype PathDataJSON = MkPathDataJSON (PathTypeW, FilePath, Timestamp, Natural)

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
      (OsPath.decode opath)

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    MkPathDataJSON (pathType, opathStr, ts, sz) <- Asn.eitherDecode $ BSL.fromStrict bs

    bimap
      displayException
      (\opath -> UnsafePathData pathType name (MkPathI opath) ts (MkBytes sz))
      (OsPath.encodeValid opathStr)

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
