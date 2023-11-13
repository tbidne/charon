{-# LANGUAGE TemplateHaskell #-}

-- | The Json backend.
module SafeRm.Backend.Json
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
    convert,
    merge,
  )
where

import SafeRm.Backend.Common qualified as Common
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Index (Index)
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend, HasTrashHome)
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
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeq (PathI TrashEntryOriginalPath) ->
  m ()
delete = Common.delete

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
    MonadPosixCompat m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
permDelete = Common.permDelete

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Index
getIndex = Common.getIndex

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Metadata
getMetadata = Common.getMetadata

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
restore = Common.restore

-- | Empties the trash.
emptyTrash ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Bool ->
  m ()
emptyTrash = Common.emptyTrash

convert ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Backend ->
  m ()
convert = Common.convert

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
merge = Common.merge

-- | Looks up the trash entry file name, throwing an exception if none is
-- found.
lookupTrashName ::
  ( HasBackend env,
    HasTrashHome env,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m (NESeq PathData)
lookupTrashName = Common.lookupTrashName
