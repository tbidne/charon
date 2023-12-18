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
import Charon.Utils qualified as Utils
import Data.Bytes qualified as Bytes
import Data.Char qualified as Ch
import Data.Text qualified as T
import Effects.System.Terminal qualified as Term
import Effects.Time (getSystemTime)
import Numeric.Algebra.Additive.AMonoid (AMonoid (zero))
import Numeric.Algebra.Additive.ASemigroup (ASemigroup ((.+.)))
import Numeric.Literal.Rational (FromRational (afromRational))

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/charon/@.
delete ::
  forall m env pd k.
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathWriter m,
    MonadReader env m,
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
  forall m env pd k.
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    HasTrashHome env,
    MonadCatch m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathWriter m,
    MonadReader env m,
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
  $(logDebug) $ "Paths: " <> USeqNE.display Paths.toText paths
  trashHome <- asks getTrashHome

  void Trash.createTrash

  someExRef <- newIORef Nothing
  currTime <- MkTimestamp <$> getSystemTime

  let deleteAction = Trash.mvOriginalToTrash backendArgs trashHome currTime

      handleEx p ex = do
        $(logError) (T.pack $ displayNoCS ex)
        putStrLn
          $ mconcat
            [ "Error deleting path '",
              decodeOsToFpDisplayEx $ p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef someExRef (Just ex)

  -- move paths to trash
  for_ paths $ \p -> do
    tryAnyCS (deleteAction p) >>= \case
      Left ex -> handleEx p ex
      Right pd -> postHook pd

  Utils.throwIfEx someExRef

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  Bool ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
permDelete backendArgs = permDeletePostHook backendArgs (const $ pure ())

-- | Permanently deletes the paths from the trash.
permDeletePostHook ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadLoggerNS m,
    MonadReader env m,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  (PathData -> m ()) ->
  Bool ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
permDeletePostHook backendArgs postHook force paths = addNamespace "permDeletePostHook" $ do
  $(logDebug) $ "Paths: " <> USeqNE.display Paths.toText paths
  trashHome <- asks getTrashHome

  someExRef <- newIORef Nothing

  -- permanently delete paths
  addNamespace "deleting" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.permDeleteFromTrash backendArgs postHook force trashHome p >>= Utils.setRefIfJust someExRef)
      `catchAnyCS` \ex -> do
        $(logError) (T.pack $ displayNoCS ex)
        putStrLn
          $ mconcat
            [ "Error permanently deleting path '",
              decodeOsToFpDisplayEx $ p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        -- in case Trash.permDeleteFromTrash throws an exception
        writeIORef someExRef (Just ex)

  Utils.throwIfEx someExRef

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  m Index
getIndex backendArgs = addNamespace "getIndex" $ do
  Trash.doesTrashExist >>= \case
    True -> Default.Index.readIndex backendArgs
    False -> do
      $(logTrace) "Trash does not exist."
      pure Index.empty

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  m Metadata
getMetadata backendArgs = addNamespace "getMetadata" $ do
  $(logTrace) "In getMetadata"
  trashHome <- asks getTrashHome
  trashLog <- Env.getTrashLog

  let MkPathI trashPathsDir = Default.Utils.getTrashPathDir trashHome

  Trash.doesTrashExist >>= \case
    False -> do
      $(logTrace) "Trash does not exist."
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
            $(logTrace) "Log does not exist"
            pure (afromRational 0)

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
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
restore backendArgs = restorePostHook backendArgs (const $ pure ())

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restorePostHook ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  (PathData -> m ()) ->
  UniqueSeqNE (PathI TrashEntryFileName) ->
  m ()
restorePostHook backendArgs postHook paths = addNamespace "restorePostHook" $ do
  $(logDebug) $ "Paths: " <> USeqNE.display Paths.toText paths
  trashHome <- asks getTrashHome

  someExRef <- newIORef Nothing

  -- move trash paths back to original location
  addNamespace "restoring" $ for_ paths $ \p ->
    -- Record error if any occurred
    (Trash.restoreTrashToOriginal backendArgs postHook trashHome p >>= Utils.setRefIfJust someExRef)
      `catchAnyCS` \ex -> do
        $(logError) (T.pack $ displayNoCS ex)
        putStrLn
          $ mconcat
            [ "Error restoring path '",
              decodeOsToFpDisplayEx $ p ^. #unPathI,
              "': ",
              displayNoCS ex
            ]
        writeIORef someExRef (Just ex)

  Utils.throwIfEx someExRef

-- | Empties the trash.
emptyTrash ::
  forall m env pd k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  Bool ->
  m ()
emptyTrash backendArgs force = addNamespace "emptyTrash" $ do
  $(logTrace) "In emptyTrash"
  trashHome@(MkPathI th) <- asks getTrashHome

  exists <- doesDirectoryExist th
  if not exists
    then do
      $(logTrace) "Trash home does not exist."
      putTextLn $ Paths.toText trashHome <> " is empty."
    else
      if force
        then do
          $(logTrace) "Force on; deleting entire trash."
          removeDirectoryRecursive th
          void Trash.createTrash
        else do
          Utils.noBuffering
          metadata <- getMetadata backendArgs
          putStrLn ""
          putTextLn $ Utils.renderPretty metadata
          putStr "Permanently delete all contents (y/n)? "
          c <- Ch.toLower <$> Term.getChar
          if
            | c == 'y' -> do
                $(logTrace) "Deleting contents."
                removeDirectoryRecursive th
                void Trash.createTrash
                putStrLn ""
            | c == 'n' -> do
                $(logTrace) "Not deleting contents."
                putStrLn ""
            | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

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
merge src dest = addNamespace "merge" $ Trash.mergeTrashDirs src dest
