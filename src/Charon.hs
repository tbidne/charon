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
import Charon.Data.Index qualified as Index
import Charon.Data.Metadata (Metadata)
import Charon.Data.PathData (PathData)
import Charon.Data.PathData.Formatting (Coloring (ColoringDetect))
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashEntryFileName, TrashHome),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.UniqueSeqNE (UniqueSeqNE, (∈))
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import Charon.Exception (BackendDetectE (MkBackendDetectE))
import Charon.Prelude
import Charon.Runner.Command.Delete (DeleteParams)
import Charon.Runner.Command.PermDelete (PermDeleteParams)
import Charon.Runner.Command.Restore (RestoreParams)
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseMerged),
    IndicesPathsStrategy (IndicesStrategy, PathsStrategy),
    Prompt,
  )
import Charon.Utils qualified as Utils
import Data.Foldable1 qualified as F1
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.FileSystem.PathWriter qualified as PW
import Text.Read qualified as TR

-- NOTE: For functions that can encounter multiple exceptions, the first
-- one is rethrown.

-- | @delete trash p@ moves path @p@ to the given trash location @trash@ and
-- writes an entry in the trash index. If the trash location is not given,
-- defaults to XDG data e.g. @~\/.local/share/charon/@.
delete ::
  forall env m k.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  DeleteParams ConfigPhaseMerged ->
  m ()
delete params = addNamespace "delete" $ do
  initalLog
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.delete prompt verbose paths
      BackendFdo -> addNamespace "fdo" $ Fdo.delete prompt verbose paths
      BackendJson -> addNamespace "json" $ Json.delete prompt verbose paths
  where
    paths = params ^. #paths
    prompt = params ^. #prompt
    verbose = params ^. #verbose

-- | Permanently deletes the paths from the trash.
permDelete ::
  forall env m k.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadIORef m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadLoggerNS m env k,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  PermDeleteParams ConfigPhaseMerged ->
  m ()
permDelete params = addNamespace "permDelete" $ do
  initalLog

  (name, idxFn, delFn) <-
    asks @env @m getBackend <&> \case
      BackendCbor -> ("cbor", Cbor.getIndex, Cbor.permDelete)
      BackendFdo -> ("fdo", Fdo.getIndex, Fdo.permDelete)
      BackendJson -> ("json", Json.getIndex, Json.permDelete)

  paths <- getIndexedPaths "delete" strategy idxFn
  addNamespace name $ delFn verbose prompt paths
  where
    strategy = params ^. #strategy
    prompt = params ^. #prompt
    verbose = params ^. #verbose

-- | Reads the index at either the specified or default location. If the
-- file does not exist, returns empty.
getIndex ::
  forall env m k.
  ( HasBackend env,
    HasCallStack,
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
  m Index
getIndex = addNamespace "getIndex" $ do
  initalLog
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" Cbor.getIndex
      BackendFdo -> addNamespace "fdo" Fdo.getIndex
      BackendJson -> addNamespace "json" Json.getIndex

-- | Retrieves metadata for the trash directory.
getMetadata ::
  forall m env k.
  ( HasBackend env,
    HasCallStack,
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
  initalLog
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" Cbor.getMetadata
      BackendFdo -> addNamespace "fdo" Fdo.getMetadata
      BackendJson -> addNamespace "json" Json.getMetadata

-- | @restore trash p@ restores the trashed path @\<trash\>\/p@ to its original
-- location.
restore ::
  forall env m k.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadHaskeline m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m,
    MonadTime m
  ) =>
  RestoreParams ConfigPhaseMerged ->
  m ()
restore params = addNamespace "restore" $ do
  initalLog

  (name, idxFn, restoreFn) <-
    asks @env @m getBackend <&> \case
      BackendCbor -> ("cbor", Cbor.getIndex, Cbor.restore)
      BackendFdo -> ("fdo", Fdo.getIndex, Fdo.restore)
      BackendJson -> ("json", Json.getIndex, Json.restore)

  paths <- getIndexedPaths "restore" (params ^. #strategy) idxFn
  addNamespace name $ restoreFn verbose force prompt paths
  where
    force = params ^. #force
    prompt = params ^. #prompt
    verbose = params ^. #verbose

-- | Empties the trash.
emptyTrash ::
  forall m env k.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHaskeline m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadPathWriter m,
    MonadPosixC m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Prompt ->
  m ()
emptyTrash prompt = addNamespace "emptyTrash" $ do
  initalLog
  asks getBackend
    >>= \case
      BackendCbor -> addNamespace "cbor" $ Cbor.emptyTrash prompt
      BackendFdo -> addNamespace "fdo" $ Fdo.emptyTrash prompt
      BackendJson -> addNamespace "json" $ Json.emptyTrash prompt

convert ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadAsync m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadIORef m,
    MonadLoggerNS m env k,
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
  initalLog

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
    MonadLoggerNS m env k,
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
  initalLog

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
    MonadLoggerNS m env k,
    MonadReader env m
  ) =>
  m ()
initalLog =
  asks getTrashHome >>= \th -> $(logDebug) ("TrashHome: " <> Paths.toText th)

-- | Retrieves trash entries based on user index input.
getIndexedPaths ::
  forall m.
  ( HasCallStack,
    MonadHaskeline m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Text ->
  IndicesPathsStrategy ->
  -- | Function to retrieve trash index.
  m Index ->
  -- | Trash entries corresponding to user indices.
  m (UniqueSeqNE (PathI TrashEntryFileName))
getIndexedPaths _ (PathsStrategy paths) _ = pure paths
getIndexedPaths actionStr IndicesStrategy idxFn = do
  index <- idxFn

  -- Sort index by original path.
  let trashIndexSeq =
        Seq.sortOn (view #originalPath)
          . fmap (view _1)
          . view #unIndex
          $ index

  -- Format the index. We use the lower-level singlelineNoSort so that we
  -- ensure we have the same sorting as above i.e. we need the indexes
  -- to line up.
  coloring <- Index.getColoring ColoringDetect
  let formatted = Index.tabularSimpleNoSort coloring trashIndexSeq
  putTextLn $ "\n" <> formatted

  putTextLn
    $ "\nPlease enter a list of space-separated indices to "
    <> actionStr
    <> "."
  putTextLn "For example: 1 3 5-12 15\n"

  txtIndices <-
    Utils.getStrippedLine "> " >>= \l ->
      case T.words l of
        [] -> throwText "Received zero indices."
        (t : ts) -> pure $ t :| ts

  -- 1. Translate ranges to indices
  indices <- foldlMapM1 parseIndexNum combineIndices txtIndices

  -- 2. Check all in range
  let maxElem = F1.maximum indices
      idxLen = length trashIndexSeq
  when (maxElem > idxLen) $ do
    let msg =
          mconcat
            [ "Maximum index ",
              showt maxElem,
              " is greater than than the trash size: ",
              showt idxLen
            ]
    throwText msg

  -- 3. Filter indices, map to trash names
  let filterIndices :: Seq PathData -> Seq (PathI TrashEntryFileName)
      filterIndices =
        fmap (view (_2 % #fileName))
          . Seq.filter (\(i, _) -> i ∈ indices)
          . Seq.zip (Seq.fromList [1 .. idxLen])

  case filterIndices trashIndexSeq of
    Seq.Empty -> throwText $ "No indices after filtering: " <> showt indices
    (x :<| xs) -> pure $ USeqNE.fromFoldable1 (x :<|| xs)
  where
    combineIndices :: UniqueSeqNE Int -> Text -> m (UniqueSeqNE Int)
    combineIndices acc txt = (acc <>) <$> parseIndexNum txt

    parseIndexNum :: Text -> m (UniqueSeqNE Int)
    parseIndexNum t = do
      when (quitTxt t) $ do
        throwM ExitSuccess
      case T.split (== '-') t of
        [one] -> do
          n <- readNumOrDie one
          pure $ USeqNE.singleton n
        [sTxt, eTxt] -> do
          s <- readNumOrDie sTxt
          e <- readNumOrDie eTxt

          case [s .. e] of
            [] -> throwText $ "Bad range: start index " <> sTxt <> " > " <> eTxt
            (x : xs) -> pure $ USeqNE.fromNonEmpty (x :| xs)
        _ -> throwText $ "Expected number or range, received: " <> t

    readNumOrDie :: Text -> m Int
    readNumOrDie t = case TR.readMaybe (T.unpack t) of
      Nothing -> throwText $ "Failed reading int: " <> t
      Just n -> pure n

    quitTxt :: Text -> Bool
    quitTxt =
      T.strip >>> T.toLower >>> \case
        "q" -> True
        ":q" -> True
        "exit" -> True
        "quit" -> True
        _ -> False
