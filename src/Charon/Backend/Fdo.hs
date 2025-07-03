{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The Fdo backend.
module Charon.Backend.Fdo
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
    isFdo,
  )
where

import Charon.Backend.Data (Backend (BackendFdo))
import Charon.Backend.Data qualified as Backend
import Charon.Backend.Default qualified as Default
import Charon.Backend.Default.Index qualified as Default.Index
import Charon.Backend.Default.Trash qualified as Default.Trash
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Backend.Fdo.BackendArgs qualified as BackendArgs
import Charon.Backend.Fdo.DirectorySizes
  ( DirectorySizes (MkDirectorySizes),
    DirectorySizesEntry
      ( MkDirectorySizesEntry,
        fileName,
        size,
        time
      ),
  )
import Charon.Backend.Fdo.DirectorySizes qualified as DirectorySizes
import Charon.Backend.Fdo.PathData qualified as Fdo.PathData
import Charon.Backend.Fdo.Utils qualified as Fdo.Utils
import Charon.Backend.Rosetta (Rosetta (MkRosetta, index, size))
import Charon.Class.Serial qualified as Serial
import Charon.Data.Index (Index)
import Charon.Data.Index qualified as Index
import Charon.Data.Metadata (Metadata)
import Charon.Data.PathData (PathData)
import Charon.Data.PathData qualified as PathData
import Charon.Data.PathType (PathTypeW)
import Charon.Data.PathType qualified as PathType
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashEntryPath,
        TrashHome
      ),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.Timestamp (Timestamp (MkTimestamp))
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Env (HasTrashHome (getTrashHome))
import Charon.Prelude
import Charon.Runner.Command (Force, NoPrompt)
import Charon.Utils qualified as Utils
import Data.HashMap.Strict qualified as HMap
import Data.Sequence qualified as Seq
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig, overwrite, targetName),
    Overwrite (OverwriteDirectories),
    TargetName (TargetNameDest),
  )
import Effects.FileSystem.PathWriter qualified as PW
import GHC.Exts (IsList (toList))
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import System.OsPath qualified as OsP

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/charon/@.
delete ::
  forall env m k.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeqNE (PathI TrashEntryOriginalPath) ->
  m ()
delete paths = do
  let appendDirectorySize :: (Fdo.PathData.PathData, PathTypeW, PathI TrashEntryPath) -> m ()
      appendDirectorySize (pd, pathType, MkPathI newPath) = addNamespace "appendDirectorySize"
        $ when (PathType.isDirectory pathType)
        $ do
          -- In FDO's directorysizes spec, the listed size does not include
          -- the directory itself. That is, while getPathSize calculates
          --
          --     size := size(dir) + size(dir contents)
          --
          -- We actually want
          --
          --     size := size(dir contents)
          --
          -- Thus we remove the dir size after performing the calculation.
          size <- Utils.getPathSize newPath
          $(logDebug) $ "Size: " <> showt size
          sizeWithoutDir <- sizeMinusDir size newPath

          let MkTimestamp localTime = pd ^. #created
              posixMillis = fromIntegral $ Utils.localTimeToMillis localTime

          fileNameEncoded <- Fdo.Utils.percentEncodeFileName pd

          let entry =
                MkDirectorySizesEntry
                  { size = sizeWithoutDir,
                    time = posixMillis,
                    fileName = fileNameEncoded
                  }

          $(logDebug) $ "DirSize entry: " <> showt entry
          DirectorySizes.appendEntry entry

  Default.deletePostHook BackendArgs.backendArgs appendDirectorySize paths

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall m env k.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m env k,
    MonadReader env m,
    MonadPosixC m,
    MonadTerminal m,
    MonadTime m
  ) =>
  NoPrompt ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
permDelete =
  Default.permDeletePostHook
    BackendArgs.backendArgs
    (addNamespace "removeDirectorySize" . removeDirectorySize)

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env m k.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m env k,
    MonadReader env m,
    MonadPosixC m,
    MonadTerminal m
  ) =>
  m Index
getIndex = addNamespace "getIndex" $ do
  trashHome <- asks getTrashHome

  Default.Trash.doesTrashExist >>= \case
    False -> do
      $(logDebug) "Trash does not exist."
      pure Index.empty
    True -> do
      MkDirectorySizes directorySizes <- DirectorySizes.readDirectorySizesTrashHome trashHome
      let dirSizesMap = HMap.fromList $ fmap (\e -> (e ^. #fileName, e)) (toList directorySizes)
          backendArgs' = BackendArgs.backendArgsDirectorySizes dirSizesMap
      Default.Index.readIndexTrashHome backendArgs' trashHome

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env k.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m Metadata
getMetadata = addNamespace "getMetadata" $ do
  MkDirectorySizes directorySizes <- DirectorySizes.readDirectorySizes
  let dirSizesMap = HMap.fromList $ fmap (\e -> (e ^. #fileName, e)) (toList directorySizes)
      backendArgs' = BackendArgs.backendArgsDirectorySizes dirSizesMap

  Default.getMetadata backendArgs'

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env m k.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadIORef m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHandleWriter m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Force ->
  NoPrompt ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
restore =
  Default.restorePostHook
    BackendArgs.backendArgs
    (addNamespace "removeDirectorySize" . removeDirectorySize)

-- | Empties the trash.
emptyTrash ::
  forall m env k.
  ( HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  NoPrompt ->
  m ()
emptyTrash = Default.emptyTrash BackendArgs.backendArgs

merge ::
  ( HasCallStack,
    HasTrashHome env,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTime m
  ) =>
  PathI TrashHome ->
  PathI TrashHome ->
  m ()
merge src dest = addNamespace "merge" $ do
  srcDirectorySizes <- DirectorySizes.readDirectorySizesTrashHome src
  destDirectorySizes <- DirectorySizes.readDirectorySizesTrashHome dest
  directorySizesPath <- DirectorySizes.getDirectorySizesPath

  tmpDirSizesPath <- Utils.getRandomTmpFile [osp|directorysizes|]

  PW.renameFile directorySizesPath tmpDirSizesPath

  PW.copyDirectoryRecursiveConfig config src' dest'
    `catchSync` \ex -> do
      PW.renameFile tmpDirSizesPath directorySizesPath
      $(logError) $ "Error merging directories: " <> displayExceptiont ex
      throwM ex

  -- If we reach here then we know the copy succeeded, hence src and dest
  -- have no clashes. Thus it is safe to combine the directorysizes
  let mergedDirectorySizes = srcDirectorySizes <> destDirectorySizes

  DirectorySizes.writeDirectorySizesTrashHome dest mergedDirectorySizes
  where
    src' = src ^. #unPathI
    dest' = dest ^. #unPathI
    config =
      MkCopyDirConfig
        { overwrite = OverwriteDirectories,
          targetName = TargetNameDest
        }

toRosetta ::
  ( HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadFileReader m,
    MonadPathReader m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m Rosetta
toRosetta = addNamespace "toRosetta" $ do
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
    MonadLoggerNS m env k,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTime m
  ) =>
  PathI TrashHome ->
  Rosetta ->
  m ()
fromRosetta tmpDir rosetta = addNamespace "fromRosetta" $ do
  $(logDebug) ("Temp dir: " <> Paths.toText tmpDir)

  -- create tmp trash
  (MkPathI trashPathDir, MkPathI trashInfoDir) <-
    Default.Trash.createTrashDir tmpDir

  mdirectorySizes <- for (rosetta ^. (#index % #unIndex)) $ \(pd, MkPathI oldTrashPath) -> do
    -- transform core path data to fdo
    let fdoPathData = Fdo.PathData.fromCorePathData pd
        newTrashPath =
          trashPathDir
            </> (fdoPathData ^. (#fileName % #unPathI))

    -- copy dir
    PathType.copyPath (pd ^. #pathType) oldTrashPath newTrashPath trashPathDir
    let msg =
          mconcat
            [ "Copying '",
              decodeDisplayExT oldTrashPath,
              "' to '",
              decodeDisplayExT newTrashPath
            ]
    $(logDebug) msg

    -- create info files
    encoded <- Serial.encodeThrowM fdoPathData
    let filePath =
          trashInfoDir
            </> (fdoPathData ^. (#fileName % #unPathI))
            <.> [osp|.trashinfo|]

    writeBinaryFile filePath encoded

    -- build directorysizes
    if PathData.isDirectory pd
      then do
        entryFileName <- Fdo.Utils.percentEncodeFileName pd

        sizeWithoutDir <- sizeMinusDir (pd ^. #size) oldTrashPath

        let time = Utils.localTimeToMillis $ pd ^. (#created % #unTimestamp)
            entry =
              MkDirectorySizesEntry
                { size = sizeWithoutDir,
                  time = fromIntegral time,
                  fileName = entryFileName
                }
        DirectorySizes.appendEntryTrashHome tmpDir entry
        pure $ Just entry
      else pure Nothing

  -- write directorysizes
  let catMaybes acc Nothing = acc
      catMaybes acc (Just x) = acc :|> x

      directorySizes =
        Seq.sortOn (\entry -> (entry ^. #time, entry ^. #fileName))
          $ foldl' catMaybes Seq.empty mdirectorySizes

  DirectorySizes.writeDirectorySizesTrashHome tmpDir (MkDirectorySizes directorySizes)

removeDirectorySize ::
  ( HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTime m
  ) =>
  PathData ->
  m ()
removeDirectorySize pd = when (PathData.isDirectory pd) (removeEntry pd)
  where
    removeEntry = Fdo.Utils.percentEncodeFileName >=> DirectorySizes.removeEntry

-- | Determines if the backend is Fdo. The semantics are if the trash is
-- a well-formed fdo backend __and__ it has at least one file with the
-- fdo extension, then we return @Just True@ (definitely true).
--
-- If the trash does not exist or is not well-formed, we return @Just False@
-- (definitely false).
--
-- If the trash __is__ a well-formed fdo backend but we do not have any files
-- we return Nothing (maybe), because we cannot tell. It __could__ be any
-- backend that shares the same trash structure, including fdo.
isFdo ::
  ( HasCallStack,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  m (Maybe Bool)
isFdo trashHome@(MkPathI th) = addNamespace "isFdo" $ do
  exists <-
    Default.Trash.doesTrashExistPath trashHome
      `catchSync` \_ -> pure False
  if exists
    then do
      let directorysizesPath = th </> [osp|directorysizes|]

      isDefinitelyFdo <- doesFileExist directorysizesPath
      if isDefinitelyFdo
        then do
          -- Trash dir contains directorysizes (fdo): Definitely true.
          $(logDebug) "Found fdo"
          pure (Just True)
        else do
          PR.listDirectory trashPath >>= \case
            -- Trash dir is well-formed but contains no files: Maybe.
            [] -> do
              $(logDebug) "Trash dir is well-formed but empty: Maybe"
              pure Nothing
            -- Trash dir has at least one file: iff ext matches fdo.
            (f : _) -> do
              let ext = OsP.takeExtension f
              $(logDebug) $ "Found file with extension " <> decodeDisplayExT ext
              pure $ Just $ Backend.backendExt BackendFdo == ext
    else do
      -- Trash does not exist or it is not a well-formed fdo backend: Definitely
      -- false.
      $(logDebug) "Unknown, not fdo"
      pure $ Just False
  where
    MkPathI trashPath = Default.Utils.getTrashInfoDir trashHome

sizeMinusDir ::
  ( HasCallStack,
    MonadLoggerNS m env k,
    MonadPathReader m
  ) =>
  -- | The base size b
  Bytes B Natural ->
  -- | The directory path d
  OsPath ->
  -- | b - dir_size(d), clamped at 0.
  m (Bytes B Natural)
sizeMinusDir size@(MkBytes sz) path = addNamespace "sizeMinusDir" $ do
  dirIntrisicSize <- fromIntegral <$> PR.getFileSize path

  if size >= MkBytes dirIntrisicSize
    then pure $ fmap (\s -> s - dirIntrisicSize) size
    else do
      let msg =
            mconcat
              [ "Directory intrinsic size (",
                showt dirIntrisicSize,
                " bytes) is somehow greater than its combined size (",
                showt sz,
                " bytes). Clamping to 0."
              ]
      $(logWarn) msg
      pure $ fromℤ 0
