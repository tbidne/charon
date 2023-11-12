{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the FreeDesktopOrg backend.
module SafeRm.Data.PathData.Fdo
  ( -- * PathData
    PathData (..),
    toPathData,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.HashSet qualified as Set
import SafeRm.Data.PathData.Common qualified as Common
import SafeRm.Data.PathType (PathType)
import SafeRm.Data.Paths
  ( PathI,
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashHome
      ),
  )
import SafeRm.Data.Serialize (Serialize (..), decodeUnit)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Prelude
import SafeRm.Utils qualified as U

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

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> Either String ByteString
  encode pd =
    case (encode (pd ^. #originalPath), encode (pd ^. #created)) of
      (Right opath, Right created) ->
        Right
          $ C8.unlines
            [ "[Trash Info]",
              "Path=" <> U.percentEncode opath,
              "DeletionDate=" <> created
            ]
      (Left ex, _) -> Left ex
      (_, Left ex) -> Left ex

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    mp <- Common.parseTrashInfoMap expectedKeys bs

    originalPath <- decodeUnit . U.percentDecode =<< Common.lookup "Path" mp
    created <- decodeUnit =<< Common.lookup "DeletionDate" mp

    Right
      $ UnsafePathData
        { fileName = name,
          originalPath,
          created
        }
    where
      expectedKeys = Set.fromList ["Path", "DeletionDate"]
