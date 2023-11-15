-- | Provides functionality for moving a file to a trash location.
module SafeRm
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

import SafeRm.Backend.Cbor qualified as Cbor
import SafeRm.Backend.Data (Backend (BackendCbor, BackendFdo, BackendJson))
import SafeRm.Backend.Fdo qualified as Fdo
import SafeRm.Backend.Json qualified as Json
import SafeRm.Data.Index (Index)
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome)
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
delete paths =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.delete paths
      BackendFdo -> addNamespace "fdo" $ Fdo.delete paths
      BackendJson -> addNamespace "json" $ Json.delete paths

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
permDelete force paths =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.permDelete force paths
      BackendFdo -> addNamespace "fdo" $ Fdo.permDelete force paths
      BackendJson -> addNamespace "json" $ Json.permDelete force paths

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
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
  m Index
getIndex =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" Cbor.getIndex
      BackendFdo -> addNamespace "fdo" Fdo.getIndex
      BackendJson -> addNamespace "json" Json.getIndex

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
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
getMetadata =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" Cbor.getMetadata
      BackendFdo -> addNamespace "fdo" Fdo.getMetadata
      BackendJson -> addNamespace "json" Json.getMetadata

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env m.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
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
restore paths =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.restore paths
      BackendFdo -> addNamespace "fdo" $ Fdo.restore paths
      BackendJson -> addNamespace "json" $ Json.restore paths

-- | Empties the trash.
emptyTrash ::
  forall m env.
  ( HasBackend env,
    HasCallStack,
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
emptyTrash force =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.emptyTrash force
      BackendFdo -> addNamespace "fdo" $ Fdo.emptyTrash force
      BackendJson -> addNamespace "json" $ Json.emptyTrash force

convert ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  Backend ->
  m ()
convert dest =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.convert dest
      BackendFdo -> addNamespace "fdo" $ Fdo.convert dest
      BackendJson -> addNamespace "json" $ Json.convert dest

merge ::
  ( HasBackend env,
    HasCallStack,
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
merge dest =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.merge dest
      BackendFdo -> addNamespace "fdo" $ Fdo.merge dest
      BackendJson -> addNamespace "json" $ Json.merge dest

lookupTrashName ::
  ( HasBackend env,
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
  UniqueSeq (PathI TrashEntryFileName) ->
  m (NESeq PathData)
lookupTrashName name =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.lookupTrashName name
      BackendFdo -> addNamespace "fdo" $ Fdo.lookupTrashName name
      BackendJson -> addNamespace "json" $ Json.lookupTrashName name
