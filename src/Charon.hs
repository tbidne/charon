{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides functionality for moving a file to a trash location.
module Charon
  ( -- * Delete
    delete,
    permDelete,
    emptyTrash,

    -- * Restore
    restore,

    -- * Information
    getIndex,
    getMetadata,

    -- * Transformations
    convert,
    merge,
  )
where

import Charon.Backend.Cbor qualified as Cbor
import Charon.Backend.Data (Backend (BackendCbor, BackendFdo, BackendJson))
import Charon.Backend.Data qualified as Backend.Data
import Charon.Backend.Fdo qualified as Fdo
import Charon.Backend.Json qualified as Json
import Charon.Data.Index (Index)
import Charon.Data.Metadata (Metadata)
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import Charon.Exception (BackendDetectE (MkBackendDetectE))
import Charon.Prelude
import Charon.Utils qualified as Utils
import Data.Text qualified as T
import Effects.FileSystem.PathWriter qualified as PW

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/charon/@.
delete ::
  forall env m.
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
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeqNE (PathI TrashEntryOriginalPath) ->
  m ()
delete paths = addNamespace "delete" $ do
  initalLog "In delete"
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
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Bool ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
permDelete force paths = addNamespace "permDelete" $ do
  initalLog "In permDelete"
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m Index
getIndex = addNamespace "getIndex" $ do
  initalLog "In getIndex"
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  initalLog "In getMetadata"
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
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
restore paths = addNamespace "restore" $ do
  initalLog "In restore"
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Bool ->
  m ()
emptyTrash force = addNamespace "emptyTrash" $ do
  initalLog "In emptyTrash"
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
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Backend ->
  m ()
convert dest = addNamespace "convert" $ do
  initalLog "In convert"

  (MkPathI trashHome') <- asks getTrashHome

  src <- asks getBackend
  if src == dest
    then do
      let msg =
            mconcat
              [ "--backend == requested conversion type: " <> Backend.Data.backendName dest,
                ". Nothing to do."
              ]
      $(logInfo) $ T.pack msg
      putStrLn msg
    else do
      $(logDebug) $ "Current backend: " <> T.pack (Backend.Data.backendName src)

      -- 1. Get Rosetta
      rosetta <- case src of
        BackendCbor -> addNamespace "cbor" Cbor.toRosetta
        BackendFdo -> addNamespace "fdo" Fdo.toRosetta
        BackendJson -> addNamespace "json" Json.toRosetta

      $(logDebug) ("Rosetta: " <> showt rosetta)

      newTrashTmpRaw <- Utils.getRandomTmpFile [osp|tmp_trash_new|]

      let newTrashTmp :: PathI TrashHome
          newTrashTmp = MkPathI newTrashTmpRaw

      fromRosettaFn newTrashTmp rosetta
        `catchSync` \ex -> do
          PW.removeDirectoryRecursiveIfExists_ newTrashTmpRaw
          $(logError) $ "Exception writing rosetta: " <> displayExceptiont ex
          throwM ex

      -- 3. Back up old trash
      oldTrashTmpRaw <- Utils.getRandomTmpFile [osp|tmp_trash_old|]

      renameDirectory trashHome' oldTrashTmpRaw
        `catchSync` \ex -> do
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
        `catchSync` \ex -> do
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
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  PathI TrashHome ->
  m ()
merge dest = addNamespace "merge" $ do
  initalLog "In merge"

  src <- asks getTrashHome

  src' <- Paths.liftPathIF' canonicalizePath src
  dest' <- Paths.liftPathIF' canonicalizePath dest

  $(logDebug) ("Merging into: " <> Paths.toText dest')

  if src' == dest'
    then do
      let msg =
            mconcat
              [ "Source path ",
                decodeDisplayEx $ src' ^. #unPathI,
                " is the same as dest path ",
                decodeDisplayEx $ dest' ^. #unPathI,
                ". Nothing to do."
              ]
      $(logInfo) $ T.pack msg
      putStrLn msg
    else do
      $(logDebug) $ "Dest path: " <> Paths.toText dest
      backend <- asks getBackend

      case backend of
        BackendCbor ->
          Cbor.isCbor dest >>= \case
            Just False -> throwM $ MkBackendDetectE BackendCbor
            _ -> addNamespace "cbor" $ Cbor.merge src' dest'
        BackendFdo ->
          Fdo.isFdo dest >>= \case
            Just False -> throwM $ MkBackendDetectE BackendFdo
            _ -> addNamespace "fdo" $ Fdo.merge src' dest'
        BackendJson ->
          Json.isJson dest >>= \case
            Just False -> throwM $ MkBackendDetectE BackendJson
            _ -> addNamespace "json" $ Json.merge src' dest'

initalLog ::
  ( HasTrashHome env,
    MonadLoggerNS m,
    MonadReader env m
  ) =>
  Text ->
  m ()
initalLog start = do
  $(logTrace) start
  asks getTrashHome >>= \th -> $(logDebug) ("TrashHome: " <> Paths.toText th)
