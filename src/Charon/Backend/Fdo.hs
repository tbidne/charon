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
import Charon.Backend.Default.Trash qualified as Trash
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Backend.Fdo.BackendArgs (backendArgs)
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
    PathIndex (TrashEntryFileName, TrashEntryOriginalPath, TrashHome),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.Timestamp (Timestamp (MkTimestamp))
import Charon.Data.UniqueSeq (UniqueSeq)
import Charon.Env (HasBackend, HasTrashHome (getTrashHome))
import Charon.Prelude
import Charon.Utils qualified as Utils
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Traversable (for)
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig, overwrite, targetName),
    Overwrite (OverwriteDirectories),
    TargetName (TargetNameDest),
  )
import Effects.FileSystem.PathWriter qualified as PW
import Effects.System.PosixCompat (_PathTypeDirectory)
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
    MonadFileReader m,
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
  UniqueSeq (PathI TrashEntryOriginalPath) ->
  m ()
delete paths = do
  trashHome <- asks getTrashHome

  let appendDirectorySize :: (Fdo.PathData.PathData, PathTypeW) -> m ()
      appendDirectorySize (pd, pathType) =
        when (is (#unPathTypeW % _PathTypeDirectory) pathType) $ do
          let MkPathI trashPath = Trash.getTrashPath trashHome (pd ^. #fileName)
          -- We could instead use the backendArgs to transform our Fdo.MkPathData
          -- into the core PathData and get its size. So why don't we? The core
          -- PathData's dir size includes the directory itself i.e.
          --
          -- size := size(dir) + size(dir contents)
          --
          -- For example, a directory with a single 4 bytes file will usually
          -- have the size 4096 (dir size) + 4 = 5000 bytes on Posix.
          --
          -- This is normally what we want e.g. calculating entire trash size
          -- and listing each entry's size.
          --
          -- But in FDO's directorysizes spec, the listed size does __not__
          -- include the directory itself. Thus we instead use
          -- 'getPathSizeIgnoreDirSize' here.
          size <- view _MkBytes <$> Utils.getPathSizeIgnoreDirSize trashPath

          let MkTimestamp localTime = pd ^. #created
              posixMillis = fromIntegral $ Utils.localTimeToMillis localTime

          fileNameEncoded <- percentEncodeFileName pd

          let entry =
                MkDirectorySizesEntry
                  { size,
                    time = posixMillis,
                    fileName = fileNameEncoded
                  }

          DirectorySizes.appendEntry entry

  Default.deletePostHook backendArgs appendDirectorySize paths

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall m env.
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
    MonadLoggerNS m,
    MonadReader env m,
    MonadPosixCompat m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Bool ->
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
permDelete =
  Default.permDeletePostHook backendArgs removeDirectorySize

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
  trashHome <- asks getTrashHome

  -- TODO: Use directorysizes in index calculation.

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
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  UniqueSeq (PathI TrashEntryFileName) ->
  m ()
restore = Default.restorePostHook backendArgs removeDirectorySize

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
    HasTrashHome env,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
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
    `catchAnyCS` \ex -> do
      PW.renameFile tmpDirSizesPath directorySizesPath
      $(logError) $ "Error merging directories: " <> displayExceptiont ex
      throwCS ex

  -- If we reach here then we know the copy succeeded, hence src and dest
  -- have no clashes. Thus it is safe to combine the directorysizes
  let mergedDirectorySizes = srcDirectorySizes <> destDirectorySizes

  DirectorySizes.writeDirectorySizesTrashHome dest mergedDirectorySizes

  $(logDebug) "Merge successful"
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
    MonadLoggerNS m,
    MonadFileReader m,
    MonadPathReader m,
    MonadPosixCompat m,
    MonadReader env m,
    MonadTerminal m
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
        entryFileName <- percentEncodeFileName pd
        let time = Utils.localTimeToMillis $ pd ^. (#created % #unTimestamp)
            entry =
              MkDirectorySizesEntry
                { size = pd ^. (#size % _MkBytes),
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
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTime m
  ) =>
  PathData ->
  m ()
removeDirectorySize pd = when (PathData.isDirectory pd) (removeEntry pd)
  where
    removeEntry = percentEncodeFileName >=> DirectorySizes.removeEntry

percentEncodeFileName ::
  forall m pd k.
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadThrow m
  ) =>
  pd ->
  m ByteString
percentEncodeFileName pd =
  percentEncode <$> decodeOsToFpThrowM fileName
  where
    percentEncode = Utils.percentEncode . encodeUtf8 . T.pack
    MkPathI fileName = view #fileName pd

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
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  m (Maybe Bool)
isFdo trashHome@(MkPathI th) = do
  exists <-
    Default.Trash.doesTrashExistPath trashHome
      `catchAnyCS` \_ -> pure False
  if exists
    then do
      let directorysizesPath = th </> [osp|directorysizes|]

      isDefinitelyFdo <- doesFileExist directorysizesPath
      if isDefinitelyFdo
        then -- Trash dir contains directorysizes (fdo): Definitely true.
          pure (Just True)
        else do
          PR.listDirectory trashPath <&> \case
            -- Trash dir is well-formed but contains no files: Maybe.
            [] -> Nothing
            -- Trash dir has at least one file: iff ext matches fdo.
            (f : _) ->
              let ext = OsP.takeExtension f
               in Just $ Backend.backendExt BackendFdo == ext
    else -- Trash does not exist or it is not a well-formed fdo backend: Definitely
    -- false.
      pure $ Just False
  where
    MkPathI trashPath = Default.Utils.getTrashInfoDir trashHome
