{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the Json backend.
module SafeRm.Data.PathData.Json
  ( -- * PathData
    PathData (..),
    toPathData,
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
import SafeRm.Data.PathData.Common qualified as Common
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
  (fileName', originalPath', pathType) <- Common.getPathInfo trashHome origPath

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
