{-# LANGUAGE TemplateHaskell #-}

-- | This module (and sub-hierarchy) provides logic that can be used by any
-- backend (i.e. cbor, fdo, json) that has the following structure:
--
-- @
--   trash/paths/ (paths)
--   trash/info/  (info files)
-- @
--
-- The only difference is in the (de)serialization.
module Charon.Backend.Default
  ( -- * Delete
    delete,
    deletePostHook,
    permDelete,
    permDeletePostHook,
    emptyTrash,

    -- * Restore
    restore,
    restorePostHook,

    -- * Information
    getIndex,
    getMetadata,

    -- * Transformations
    merge,
  )
where

import Charon.Backend.Default.BackendArgs (BackendArgs)
import Charon.Backend.Default.Index qualified as Default.Index
import Charon.Backend.Default.Trash qualified as Trash
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Class.Serial (Serial (DecodeExtra))
import Charon.Data.Index (Index)
import Charon.Data.Index qualified as Index
import Charon.Data.Metadata (Metadata (MkMetadata))
import Charon.Data.Metadata qualified as Metadata
import Charon.Data.PathData (PathData)
import Charon.Data.PathData.Formatting
  ( Coloring (ColoringDetect),
    PathDataFormat (FormatTabular),
    Sort (Name),
  )
import Charon.Data.PathType (PathTypeW)
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
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Env (HasTrashHome (getTrashHome))
import Charon.Env qualified as Env
import Charon.Prelude
import Charon.Runner.Command (Force, NoPrompt)
import Charon.Runner.Command.List
  ( ListCmd (MkListCmd, format, revSort, sort),
  )
import Charon.Utils qualified as Utils
import Data.Bytes qualified as Bytes
import Data.Sequence qualified as Seq
import Effects.Time (getSystemTime)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/charon/@.
delete ::
  forall m env pd k1 k2.
  ( HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k1 pd (PathI TrashEntryOriginalPath),
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m env k2,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  UniqueSeqNE (PathI TrashEntryOriginalPath) ->
  m ()
delete backendArgs = deletePostHook backendArgs (const $ pure ())

-- | 'delete' that takes a callback that runs on the created path data, assuming
-- the delete succeeded.
deletePostHook ::
  forall m env pd k1 k2.
  ( HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k1 pd (PathI TrashEntryOriginalPath),
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m env k2,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  ((pd, PathTypeW, PathI TrashEntryPath) -> m ()) ->
  UniqueSeqNE (PathI TrashEntryOriginalPath) ->
  m ()
deletePostHook backendArgs postHook paths = addNamespace "deletePostHook" $ do
  $(logDebug) $ "Paths: " <> USeqNE.displayUSeqNE Paths.toText paths
  trashHome <- asks getTrashHome

  void Trash.createTrash

  currTime <- MkTimestamp <$> getSystemTime

  let deleteAction = Trash.mvOriginalToTrash backendArgs trashHome currTime

  deletedPathsRef <- newIORef Seq.Empty

  -- move paths to trash
  eResult <- trySync $ for_ paths $ \p -> do
    pd <- deleteAction p
    modifyIORef' deletedPathsRef (:|> p)
    postHook pd

  deletedPaths <- readIORef deletedPathsRef

  unless (null deletedPaths) $ do
    let msg = Utils.displayList Paths.renderPath deletedPaths

    putTextLn
      $ mconcat
        [ "Deleted paths:",
          msg
        ]

  case eResult of
    Right _ -> pure ()
    Left ex -> throwM ex

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHaskeline m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m env k2,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  NoPrompt ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
permDelete backendArgs = permDeletePostHook backendArgs (const $ pure ())

-- | Permanently deletes the paths from the trash.
permDeletePostHook ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHaskeline m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m env k2,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  (PathData -> m ()) ->
  NoPrompt ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
permDeletePostHook backendArgs postHook noPrompt paths = addNamespace "permDeletePostHook" $ do
  $(logDebug) $ "Paths: " <> USeqNE.displayUSeqNE Paths.toText paths
  trashHome <- asks getTrashHome

  deletedPathsRef <- newIORef Seq.Empty

  -- permanently delete paths
  addNamespace "deleting" $ do
    let deleteAction =
          Trash.permDeleteFromTrash
            backendArgs
            postHook
            noPrompt
            deletedPathsRef
            trashHome

    eResult <- trySync $ for_ paths deleteAction

    deletedPaths <- readIORef deletedPathsRef

    unless (null deletedPaths) $ do
      let msg = Utils.displayList Paths.renderPath deletedPaths

      putTextLn
        $ mconcat
          [ "Permanently deleted paths:",
            msg
          ]

    case eResult of
      Right _ -> pure ()
      Left ex -> throwM ex

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m env k2,
    Serial pd
  ) =>
  BackendArgs m pd ->
  m Index
getIndex backendArgs = addNamespace "getIndex" $ do
  Trash.doesTrashExist >>= \case
    True -> Default.Index.readIndex backendArgs
    False -> do
      $(logDebug) "Trash does not exist."
      pure Index.empty

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k2,
    MonadPathReader m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  m Metadata
getMetadata backendArgs = addNamespace "getMetadata" $ do
  trashHome <- asks getTrashHome
  trashLog <- Env.getTrashLog

  let MkPathI trashPathsDir = Default.Utils.getTrashPathDir trashHome

  Trash.doesTrashExist >>= \case
    False -> do
      $(logDebug) "Trash does not exist."
      pure Metadata.empty
    True -> do
      -- Index size
      index <- view #unIndex <$> Default.Index.readIndexTrashHome backendArgs trashHome
      let numIndex = length index
      $(logDebug) ("Index size: " <> showt numIndex)

      -- Num entries
      numEntries <- foldl' (\acc _ -> acc + 1) 0 <$> listDirectory trashPathsDir
      $(logDebug) ("Num entries: " <> showt numEntries)

      -- Log size
      let logPath = trashLog ^. #unPathI
      logExists <- doesFileExist logPath
      logSize <-
        if logExists
          then Bytes.normalize . toDouble . MkBytes @B <$> getFileSize logPath
          else do
            $(logDebug) "Log does not exist"
            pure (fromℚ 0)

      -- TODO: Utils.getAllFiles is unfortunately expensive for many files
      -- (e.g. ~10s for 120,000 files). Maybe PosixCompat can help?

      -- Summed size
      allFiles <- Utils.getAllFiles trashPathsDir
      let allSizes = foldl' (\acc (pd, _) -> (pd ^. #size) .+. acc) zero index
          numFiles = length allFiles
          size = fromIntegral <$> Bytes.normalize allSizes

      $(logDebug) ("Num all files: " <> showt numFiles)
      $(logDebug) ("Total size: " <> showt size)

      -- NOTE: If the index is successfully read then we have verified that
      -- all invariants are preserved i.e. bijection between /files and /info.

      pure
        $ MkMetadata
          { numEntries = toNat numEntries,
            numFiles = toNat numFiles,
            logSize,
            size
          }
  where
    toDouble :: (Integral a) => Bytes s a -> Bytes s Double
    toDouble = fmap fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadHaskeline m,
    MonadIORef m,
    MonadLoggerNS m env k2,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  Force ->
  NoPrompt ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
restore backendArgs = restorePostHook backendArgs (const $ pure ())

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restorePostHook ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadHaskeline m,
    MonadIORef m,
    MonadLoggerNS m env k2,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  (PathData -> m ()) ->
  Force ->
  NoPrompt ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
restorePostHook backendArgs postHook force noPrompt paths = addNamespace "restorePostHook" $ do
  $(logDebug) $ "Paths: " <> USeqNE.displayUSeqNE Paths.toText paths
  trashHome <- asks getTrashHome

  restoredPathsRef <- newIORef Seq.Empty

  -- move trash paths back to original location
  addNamespace "restoring" $ do
    let restoreAction =
          Trash.restoreTrashToOriginal
            backendArgs
            postHook
            force
            noPrompt
            restoredPathsRef
            trashHome

    eResult <- trySync $ for_ paths restoreAction

    restoredPaths <- readIORef restoredPathsRef

    unless (null restoredPaths) $ do
      let msg = Utils.displayList Paths.renderPath restoredPaths

      putTextLn
        $ mconcat
          [ "Restored paths:",
            msg
          ]

    case eResult of
      Right _ -> pure ()
      Left ex -> throwM ex

-- | Empties the trash.
emptyTrash ::
  forall m env pd k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHaskeline m,
    MonadLoggerNS m env k2,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  NoPrompt ->
  m ()
emptyTrash backendArgs noPrompt = addNamespace "emptyTrash" $ do
  trashHome@(MkPathI th) <- asks getTrashHome

  exists <- doesDirectoryExist th
  if not exists
    then do
      $(logDebug) "Trash home does not exist."
      putTextLn $ Paths.toText trashHome <> " is empty."
    else
      if noPrompt ^. #unNoPrompt
        then do
          $(logDebug) "--no-prompt on; deleting entire trash."
          removeDirectoryRecursive th
          void Trash.createTrash
        else do
          index <- getIndex backendArgs
          indexTxt <- Index.formatIndex indexFormat index
          Utils.putLine
          putTextLn indexTxt

          metadata <- getMetadata backendArgs
          Utils.putLine
          putTextLn $ Utils.renderPretty metadata
          ans <- Utils.askYesNoQ "Permanently delete all contents"
          if ans
            then do
              $(logDebug) "Deleting contents."
              removeDirectoryRecursive th
              void Trash.createTrash
              Utils.putLine
            else do
              $(logDebug) "Not deleting contents."
              Utils.putLine
  where
    indexFormat =
      MkListCmd
        { format = FormatTabular ColoringDetect Nothing Nothing,
          sort = Name,
          revSort = False
        }

merge ::
  ( HasCallStack,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathI TrashHome ->
  PathI TrashHome ->
  m ()
merge src dest = addNamespace "merge" $ Trash.mergeTrashDirs src dest
