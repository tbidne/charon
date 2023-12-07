{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Cbor backend.
module Charon.Backend.Cbor
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
    merge,

    -- * Rosetta
    toRosetta,
    fromRosetta,

    -- * Existence
    isCbor,
  )
where

import Charon.Backend.Cbor.BackendArgs (backendArgs)
import Charon.Backend.Cbor.PathData qualified as Cbor.PathData
import Charon.Backend.Data (Backend (BackendCbor))
import Charon.Backend.Data qualified as Backend
import Charon.Backend.Default qualified as Default
import Charon.Backend.Default.Index qualified as Default.Index
import Charon.Backend.Default.Trash qualified as Default.Trash
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Backend.Rosetta (Rosetta (MkRosetta, index, size))
import Charon.Class.Serial qualified as Serial
import Charon.Data.Index (Index)
import Charon.Data.Index qualified as Index
import Charon.Data.Metadata (Metadata)
import Charon.Data.PathType qualified as PathType
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Env (HasBackend, HasTrashHome)
import Charon.Prelude
import Effects.FileSystem.PathReader qualified as PR
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import System.OsPath qualified as OsP

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
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeqNE (PathI TrashEntryOriginalPath) ->
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
    MonadTerminal m
  ) =>
  Bool ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
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
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadPosixCompat m,
    MonadTerminal m
  ) =>
  m Index
getIndex = addNamespace "getIndex" $ do
  $(logTrace) "In getIndex"

  Default.Trash.doesTrashExist >>= \case
    False -> do
      $(logTrace) "Trash does not exist"
      pure Index.empty
    True -> Default.Index.readIndex backendArgs

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m
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
    MonadTerminal m
  ) =>
  UniqueSeqNE (PathI TrashEntryFileName) ->
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Bool ->
  m ()
emptyTrash = Default.emptyTrash backendArgs

merge ::
  ( HasCallStack,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathI TrashHome ->
  PathI TrashHome ->
  m ()
merge = Default.merge

toRosetta ::
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m,
    MonadFileReader m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m Rosetta
toRosetta = addNamespace "toRosetta" $ do
  $(logTrace) "In toRosetta"

  index <- getIndex
  $(logDebug) ("Index: " <> showt index)

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
  $(logTrace) "In fromRosetta"
  $(logDebug) ("Temp dir: " <> Paths.toText tmpDir)

  -- create tmp trash
  (MkPathI trashPathDir, MkPathI trashInfoDir) <-
    Default.Trash.createTrashDir tmpDir

  for_ (rosetta ^. (#index % #unIndex)) $ \(pd, MkPathI oldTrashPath) -> do
    -- transform core path data to cbor
    let cborPathData = Cbor.PathData.fromCorePathData pd
        newTrashPath =
          trashPathDir
            </> (cborPathData ^. (#fileName % #unPathI))

    -- copy dir
    PathType.copyPath (pd ^. #pathType) oldTrashPath newTrashPath trashPathDir
    let msg =
          mconcat
            [ "Copying '",
              decodeOsToFpDisplayExT oldTrashPath,
              "' to '",
              decodeOsToFpDisplayExT newTrashPath
            ]
    $(logDebug) msg

    -- create info files
    encoded <- Serial.encodeThrowM cborPathData
    let filePath =
          trashInfoDir
            </> (cborPathData ^. (#fileName % #unPathI))
            <.> [osp|.cbor|]

    writeBinaryFile filePath encoded

-- | Determines if the backend is Cbor. The semantics are if the trash is
-- a well-formed cbor backend __and__ it has at least one file with the
-- extension .cbor, then we return @Just True@ (definitely true).
--
-- If the trash does not exist or is not well-formed, we return @Just False@
-- (definitely false).
--
-- If the trash __is__ a well-formed cbor backend but we do not have any files
-- we return Nothing (maybe), because we cannot tell. It __could__ be any
-- backend that shares the same trash structure, including cbor.
isCbor ::
  ( HasCallStack,
    MonadCatch m,
    MonadLoggerNS m,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  m (Maybe Bool)
isCbor trashHome@(MkPathI th) = addNamespace "isCbor" $ do
  $(logTrace) "In isCbor"
  exists <-
    Default.Trash.doesTrashExistPath trashHome
      `catchAnyCS` \_ -> pure False
  if exists
    then do
      let directorysizesPath = th </> [osp|directorysizes|]

      isDefinitelyFdo <- doesFileExist directorysizesPath
      if isDefinitelyFdo
        then do
          -- Trash dir contains directorysizes (fdo): Definitely false.
          $(logTrace) "Found fdo"
          pure (Just False)
        else do
          PR.listDirectory trashPath >>= \case
            -- Trash dir is well-formed but contains no files: Maybe.
            [] -> do
              $(logTrace) "Trash dir is well-formed but empty: Maybe"
              pure Nothing
            -- Trash dir has at least one file: iff ext matches cbor.
            (f : _) -> do
              let ext = OsP.takeExtension f
              $(logTrace) $ "Found file with extension " <> decodeOsToFpDisplayExT ext
              pure $ Just $ Backend.backendExt BackendCbor == ext
    else do
      -- Trash does not exist or it is not a well-formed cbor backend: Definitely
      -- false.
      $(logTrace) "Unknown, not cbor"
      pure $ Just False
  where
    MkPathI trashPath = Default.Utils.getTrashInfoDir trashHome
