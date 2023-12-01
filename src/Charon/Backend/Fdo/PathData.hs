{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'PathData' for use with the FreeDesktopOrg backend.
module Charon.Backend.Fdo.PathData
  ( -- * PathData
    PathData (..),
    toPathData,
    toCorePathData,
    fromCorePathData,
  )
where

import Charon.Backend.Default.Trash qualified as Trash
import Charon.Backend.Default.Utils qualified as Default.Utils
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
import Charon.Data.Timestamp (Timestamp)
import Charon.Prelude
import Charon.Utils qualified as U
import Charon.Utils qualified as Utils
import Data.ByteString.Char8 qualified as C8
import Data.HashSet qualified as Set

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
    MonadLoggerNS m,
    MonadPathReader m
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
    mp <- Default.Utils.parseTrashInfoMap expectedKeys bs

    originalPath <- decodeUnit . U.percentDecode =<< Default.Utils.lookup "Path" mp
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  PathData ->
  m PathData.PathData
toCorePathData trashHome pd = do
  pathType <- Default.Utils.pathDataToType trashHome pd

  -- TODO: Since we now have directorysizes, there is a potentially faster
  -- solution. For files / symlinks, simply call getFileSize / symlink equiv.
  -- For directories, read directorysizes and find the right entry, falling
  -- back to getPathSize as a last resort.
  --
  -- We would want to modify this function to take in the DirectorySizes since
  -- we do not want to calculate that on the fly all the time (and probably
  -- store it in a set e.g. UniqueSeq).
  --
  -- The problem is toCorePathData is used in several places, and we may not
  -- want to read directorysizes in all of them.
  --
  -- We should first consider if there is a nicer way to refactor the
  -- backendArgs stuff.
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
  PathData.PathData ->
  PathData
fromCorePathData pd =
  UnsafePathData
    { fileName = pd ^. #fileName,
      originalPath = pd ^. #originalPath,
      created = pd ^. #created
    }
