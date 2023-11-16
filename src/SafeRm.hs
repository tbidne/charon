{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.Text qualified as T
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter qualified as PW
import SafeRm.Backend.Cbor qualified as Cbor
import SafeRm.Backend.Data (Backend (BackendCbor, BackendFdo, BackendJson))
import SafeRm.Backend.Data qualified as Backend.Data
import SafeRm.Backend.Fdo qualified as Fdo
import SafeRm.Backend.Json qualified as Json
import SafeRm.Data.Index (Index)
import SafeRm.Data.Metadata (Metadata)
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
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
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadThread m
  ) =>
  Backend ->
  m ()
convert dest = addNamespace "convert" $ do
  trashHome@(MkPathI trashHome') <- asks getTrashHome
  $(logDebug) ("Trash home: " <> Paths.toText trashHome)

  src <- asks getBackend
  if src == dest
    then do
      let msg =
            mconcat
              [ "--backend == requested conversion type: " <> Backend.Data.backendName dest,
                ". Nothing to do."
              ]
      $(logDebug) $ T.pack msg
      putStrLn msg
    else do
      $(logDebug) $ "Current backend: " <> T.pack (Backend.Data.backendName src)

      -- 1. Get Rosetta
      rosetta <- case src of
        BackendCbor -> addNamespace "cbor" Cbor.toRosetta
        BackendFdo -> addNamespace "fdo" Fdo.toRosetta
        BackendJson -> addNamespace "json" Json.toRosetta

      -- 2. Make new tmp trash
      tmpDir <- PR.getTemporaryDirectory
      let newTrashTmpRaw = tmpDir </> [osp|tmp_trash_new|]

          newTrashTmp :: PathI TrashHome
          newTrashTmp = MkPathI newTrashTmpRaw

      fromRosettaFn newTrashTmp rosetta
        `catchAny` \ex -> do
          PW.removeDirectoryRecursiveIfExists newTrashTmpRaw
          $(logError) $ "Exception writing rosetta: " <> displayExceptiont ex
          throwM ex

      -- 3. Back up old trash
      let oldTrashTmpRaw = tmpDir </> [osp|tmp_trash_old|]

      renameDirectory trashHome' oldTrashTmpRaw
        `catchAny` \ex -> do
          let msg =
                mconcat
                  [ "Exception moving old trash dir:\n",
                    displayException ex
                  ]
          $(logError) $ T.pack msg
          putStrLn msg
          -- cleanup: remove newTrashTmp
          removeDirectoryRecursive newTrashTmpRaw
          throwM ex

      -- 4. Move newTrashTmp -> trash
      renameDirectory newTrashTmpRaw trashHome'
        `catchAny` \ex -> do
          let msg =
                mconcat
                  [ "Exception moving new trash dir:\n",
                    displayException ex
                  ]
          $(logError) $ T.pack msg
          putStrLn msg
          -- cleanup: remove newTrashTmp, move oldTrashTmpRaw back
          removeDirectoryRecursive newTrashTmpRaw
          renameDirectory oldTrashTmpRaw trashHome'
          throwM ex

      -- 4. Delete oldTrashTmpRaw
      removeDirectoryRecursive oldTrashTmpRaw
  where
    fromRosettaFn th r = case dest of
      BackendCbor -> addNamespace "cbor" $ Cbor.fromRosetta th r
      BackendFdo -> addNamespace "fdo" $ Fdo.fromRosetta th r
      BackendJson -> addNamespace "json" $ Json.fromRosetta th r

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
  UniqueSeq (PathI TrashEntryFileName) ->
  m (NESeq PathData)
lookupTrashName name =
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.lookupTrashName name
      BackendFdo -> addNamespace "fdo" $ Fdo.lookupTrashName name
      BackendJson -> addNamespace "json" $ Json.lookupTrashName name
