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
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy qualified as BSL
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env qualified as Env
import SafeRm.Exception (FileNotFoundE (..), PathNotFileDirE (..))
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

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> ByteString
  encode (UnsafePathData _ (MkPathI opath) ts) =
    BSL.toStrict $ Serialise.serialise (opath, ts)

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    (originalPath, created) <- first show $ Serialise.deserialiseOrFail (BSL.fromStrict bs)

    Right $
      UnsafePathData
        { fileName = name,
          originalPath = MkPathI originalPath,
          created
        }

-- | Derives the 'PathType' from the 'PathData'.
--
-- __IMPORTANT:__ This function is only guaranteed to work if the 'PathData'
-- corresponds to an extant trash entry. In particular, if the 'PathData' has
-- not been created yet, this can fail.
pathDataToType ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m PathType
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
            then throwCS $ MkPathNotFileDirE path
            else throwCS $ MkFileNotFoundE path
  where
    MkPathI path = Env.getTrashPath trashHome (pd ^. #fileName)
