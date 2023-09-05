{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the Cbor backend.
module SafeRm.Data.PathData.Cbor
  ( -- * PathData
    PathData (..),
    toPathData,

    -- * Misc
    pathDataToType,
  )
where

import Codec.Serialise qualified as Serialise
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Lazy qualified as BSL
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
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
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( FileNotFoundE (MkFileNotFoundE),
    PathNotFileDirE (MkPathNotFileDirE),
  )
import SafeRm.Prelude

-- | Data for an Fdo path. Maintains an invariant that the original path is not
-- the root nor is it empty.
data PathData = UnsafePathData
  { -- | The path to be used in the trash directory.
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | Time this entry was created.
    created :: !Timestamp
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
  ( LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  Eff es (PathData, PathType)
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

-- | Derives the 'PathType' from the 'PathData'.
--
-- __IMPORTANT:__ This function is only guaranteed to work if the 'PathData'
-- corresponds to an extant trash entry. In particular, if the 'PathData' has
-- not been created yet, this can fail.
pathDataToType ::
  ( PathReaderDynamic :> es
  ) =>
  PathI TrashHome ->
  PathData ->
  Eff es PathType
pathDataToType trashHome pd = do
  fileExists <- doesFileExist path
  if fileExists
    then pure PathTypeFile
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          -- for a better error message
          pathExists <- doesPathExist path
          if pathExists
            then throwM $ MkPathNotFileDirE path
            else throwM $ MkFileNotFoundE path
  where
    MkPathI path = Env.getTrashPath trashHome (pd ^. #fileName)
