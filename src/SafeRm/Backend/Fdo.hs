{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Fdo backend.
module SafeRm.Backend.Fdo
  ( -- * Delete
    delete,
    permDelete,
    emptyTrash,

    -- * Restore
    restore,

    -- * Information
    lookupTrashName,
    getIndex,
    getMetadata,

    -- * Transformations
    merge,

    -- * Rosetta
    toRosetta,
    fromRosetta,
  )
where

import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import SafeRm.Backend.Default qualified as Default
import SafeRm.Backend.Default.Index qualified as Default.Index
import SafeRm.Backend.Default.Trash qualified as Default.Trash
import SafeRm.Backend.Fdo.BackendArgs (backendArgs)
import SafeRm.Backend.Fdo.PathData qualified as Fdo.PathData
import SafeRm.Backend.Rosetta (Rosetta (MkRosetta, index, size))
import SafeRm.Data.Index (Index)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathType qualified as PathType
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize qualified as Serialize
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend, HasTrashHome (getTrashHome))
import SafeRm.Prelude

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/safe-rm/@.
delete ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m,
    MonadTime m
  ) =>
  UniqueSeq (PathI TrashEntryOriginalPath) ->
  m ()
delete = Default.delete backendArgs

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadThread m
  ) =>
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
permDelete = Default.permDelete backendArgs

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env m.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadThread m
  ) =>
  m Index
getIndex = addNamespace "getIndex" $ do
  trashHome <- asks getTrashHome

  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  Default.Trash.doesTrashExist >>= \case
    False -> do
      $(logDebug) "Trash does not exist."
      pure Index.empty
    True -> Default.Index.readIndex backendArgs trashHome

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  m Metadata
getMetadata = Default.getMetadata backendArgs

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
restore = Default.restore backendArgs

-- | Empties the trash.
emptyTrash ::
  forall m env.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  Bool ->
  m ()
emptyTrash = Default.emptyTrash backendArgs

merge ::
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  m ()
merge = Default.merge

-- | Looks up the trash entry file name, throwing an exception if none is
-- found.
lookupTrashName ::
  ( HasBackend env,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadThread m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m (NESeq PathData)
lookupTrashName = Default.lookupTrashName backendArgs

toRosetta ::
  ( HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadIORef m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  m Rosetta
toRosetta = addNamespace "toRosetta" $ do
  trashHome <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  index <- getIndex

  let size = foldl' (\acc (pd, _) -> acc .+. pd ^. #size) zero (index ^. #unIndex)

  pure
    $ MkRosetta
      { index,
        size
      }

fromRosetta ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathI TrashHome ->
  Rosetta ->
  m ()
fromRosetta tmpDir rosetta = addNamespace "fromRosetta" $ do
  $(logDebug) ("Temp dir: " <> Paths.toText tmpDir)

  -- create tmp trash
  (MkPathI trashPathDir, MkPathI trashInfoDir) <-
    Default.Trash.createTrashDir tmpDir

  for_ (rosetta ^. (#index % #unIndex)) $ \(pd, MkPathI oldTrashPath) -> do
    -- transform core path data to fdo
    fdoPathData <- Fdo.PathData.fromCorePathData pd
    let newTrashPath =
          trashPathDir
            </> (fdoPathData ^. (#fileName % #unPathI))

    -- copy dir
    PathType.copyPath (pd ^. #pathType) oldTrashPath newTrashPath trashPathDir

    -- create info files
    encoded <- Serialize.encodeThrowM fdoPathData
    let filePath =
          trashInfoDir
            </> (fdoPathData ^. (#fileName % #unPathI))
            <.> [osp|.trashinfo|]

    writeBinaryFile filePath encoded
