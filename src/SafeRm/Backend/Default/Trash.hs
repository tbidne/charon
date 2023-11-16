{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interface for the default trash directory
module SafeRm.Backend.Default.Trash
  ( -- * Trash directory
    createTrash,
    doesTrashExist,

    -- * Main actions
    mvOriginalToTrash,
    restoreTrashToOriginal,
    permDeleteFromTrash,

    -- * Transformations
    convertBackend,
    mergeTrashDirs,

    -- * Utils
    findPathData,
    getTrashPath,
  )
where

import Data.Char qualified as Ch
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteDirectories),
    TargetName (TargetNameDest),
  )
import Effects.FileSystem.PathWriter qualified as PW
import Effects.FileSystem.PathWriter qualified as WDir
import Effects.System.Terminal qualified as Term
import SafeRm.Backend.Data (Backend)
import SafeRm.Backend.Data qualified as Backend.Data
import SafeRm.Backend.Default.BackendArgs (BackendArgs)
import SafeRm.Backend.Default.Index qualified as Default.Index
import SafeRm.Backend.Default.Utils qualified as Default.Utils
import SafeRm.Data.Index (Index (MkIndex))
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData.Core
import SafeRm.Data.PathType qualified as PathType
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryInfo,
        TrashEntryOriginalPath,
        TrashEntryPath,
        TrashHome
      ),
    (<//>),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize (Serialize (DecodeExtra, decode), encodeThrowM)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import SafeRm.Exception
  ( InfoDecodeE (MkInfoDecodeE),
    RestoreCollisionE (MkRestoreCollisionE),
    TrashDirFilesNotFoundE (MkTrashDirFilesNotFoundE),
    TrashDirInfoNotFoundE (MkTrashDirInfoNotFoundE),
    TrashEntryFileNotFoundE (MkTrashEntryFileNotFoundE),
    TrashEntryNotFoundE (MkTrashEntryNotFoundE),
    TrashEntryWildcardNotFoundE (MkTrashEntryWildcardNotFoundE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils

-- | Creates the trash directory if it does not exist.
createTrash ::
  ( HasCallStack,
    HasTrashHome env,
    MonadPathWriter m,
    MonadReader env m
  ) =>
  m ()
createTrash = do
  trashHome <- asks getTrashHome
  let trashPathDir = Default.Utils.getTrashPathDir trashHome
      trashInfoDir = Default.Utils.getTrashInfoDir trashHome

  Paths.applyPathI (createDirectoryIfMissing False) trashHome
  Paths.applyPathI (createDirectoryIfMissing False) trashPathDir
  Paths.applyPathI (createDirectoryIfMissing False) trashInfoDir

-- | Returns 'False' if @\<trash-home\>@ does not exist. If @\<trash-home\>@
-- /does/ exist but is "badly-formed" i.e. one of
--
-- * \<trash-home\>/files
-- * \<trash-home\>/info
--
-- does not, throws 'TrashDirFilesNotFoundE' or 'TrashDirInfoNotFoundE'.
--
-- If all three dirs exist, returns 'True'.
doesTrashExist ::
  ( HasCallStack,
    HasTrashHome env,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Bool
doesTrashExist = do
  trashHome <- asks getTrashHome
  let MkPathI trashPathDir' = Default.Utils.getTrashPathDir trashHome
      MkPathI trashInfoDir' = Default.Utils.getTrashInfoDir trashHome

  homeExists <- doesDirectoryExist (trashHome ^. #unPathI)
  if not homeExists
    then pure False
    else do
      pathExists <- doesDirectoryExist trashPathDir'
      infoExists <- doesDirectoryExist trashInfoDir'

      case (pathExists, infoExists) of
        -- Everything exists -> true
        (True, True) -> pure True
        -- Info and Path both do not exist -> false
        (False, False) -> pure False
        -- Path exists; info does not -> Badly formed, throw exception
        (True, False) ->
          throwCS
            $ MkTrashDirInfoNotFoundE trashHome
        -- Info exists; path does not -> Badly formed, throw exception
        (False, True) ->
          throwCS
            $ MkTrashDirFilesNotFoundE trashHome

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
mvOriginalToTrash ::
  ( HasBackend env,
    HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathWriter m,
    MonadReader env m,
    Serialize pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  Timestamp ->
  PathI TrashEntryOriginalPath ->
  m ()
mvOriginalToTrash backendArgs trashHome currTime path = addNamespace "mvOriginalToTrash" $ do
  backend <- asks getBackend
  (pd, pathType) <- (backendArgs ^. #toPd) currTime trashHome path
  $(logDebug) ("Deleting: " <> showt pd)

  let fileName = pd ^. #fileName
      MkPathI trashPath = getTrashPath trashHome fileName
      MkPathI trashInfoPath = getTrashInfoPath backend trashHome fileName

  -- 2. Write info file
  --
  -- Perform this before the actual move to be safe i.e. path is only moved
  -- if info is already created.
  encoded <- encodeThrowM pd
  writeBinaryFile trashInfoPath encoded

  -- 4. Move file to trash
  let MkPathI opath = pd ^. #originalPath
      moveFn = PathType.renameFn pathType opath trashPath

  -- 5. If move failed, roll back info file
  moveFn `catchAny` \ex -> do
    $(logError) ("Error moving file to trash: " <> displayExceptiont ex)
    PW.removeFile trashInfoPath
    throwM ex

  $(logDebug) ("Moved to trash: " <> showt pd)

-- | Permanently deletes the trash path. Returns 'True' if any deletes fail.
-- In this case, the error has already been reported, so this is purely for
-- signaling (i.e. should we exit with an error).
permDeleteFromTrash ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadAsync m,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  Bool ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (Maybe SomeException)
permDeleteFromTrash backendArgs force trashHome pathName = addNamespace "permDeleteFromTrash" $ do
  pathDatas <- findPathData backendArgs trashHome pathName
  backend <- asks getBackend

  anyExRef <- newIORef Nothing
  let deleteFn pathData = do
        $(logDebug) ("Deleting: " <> Paths.toText (pathData ^. #fileName))
        if force
          then -- NOTE: Technically don't need the pathdata if force is on, since we have
          -- the path and can just delete it. Nevertheless, we retrieve the pathData
          -- so that force does not change the semantics i.e. can only delete
          -- "well-behaved" files, and we don't have to do a redundant file/directory
          -- check.
            deleteFn' backend pathData
          else do
            -- NOTE:
            -- - No buffering on input so we can read a single char w/o requiring a
            --   newline to end the input (which then gets passed to getChar, which
            --   interferes with subsequent calls).
            --
            -- - No buffering on output so the "Permanently delete..." string gets
            --   printed w/o the newline.
            Utils.noBuffering

            let pdStr = Utils.renderPretty pathData
            putTextLn pdStr
            putStr "\nPermanently delete (y/n)? "
            c <- Ch.toLower <$> Term.getChar
            if
              | c == 'y' -> deleteFn' backend pathData *> putStrLn "\n"
              | c == 'n' -> putStrLn "\n"
              | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

  -- Need our own error handling here since if we are deleting multiple
  -- wildcard matches we want success/failure to be independent.
  for_ pathDatas $ \pathData ->
    deleteFn pathData `catchAnyCS` \ex -> do
      writeIORef anyExRef (Just ex)
      $(logWarn) (T.pack $ displayNoCS ex)
      putStrLn
        $ mconcat
          [ "Error deleting path ",
            show $ pathData ^. (#fileName % #unPathI),
            ": ",
            displayNoCS ex
          ]
  readIORef anyExRef
  where
    deleteFn' b pd = do
      let MkPathI trashInfoPath' = getTrashInfoPath b trashHome (pd ^. #fileName)

      deleteFileName trashHome pd
      $(logDebug) ("Deleted: " <> showt pd)
      PW.removeFile trashInfoPath'

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
-- Returns 'True' if any failed. In this case, the error has already been
-- reported, so this is purely for signaling (i.e. should we exit with
-- an error).
restoreTrashToOriginal ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
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
    Serialize pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (Maybe SomeException)
restoreTrashToOriginal backendArgs trashHome pathName = addNamespace "restoreTrashToOriginal" $ do
  -- 1. Get path info
  pathDatas <- findPathData backendArgs trashHome pathName
  backend <- asks getBackend

  someExRef <- newIORef Nothing
  let restoreFn pd = do
        let originalPath = pd ^. #originalPath
            fileName = pd ^. #fileName

        $(logDebug) ("Restoring: " <> Paths.toText fileName)

        -- 2. Verify original path is empty
        exists <- PathData.Core.originalPathExists pd
        when exists
          $ throwCS
          $ MkRestoreCollisionE fileName originalPath

        let pathType = pd ^. #pathType
        restoreFn' backend pathType pd

  -- Need our own error handling here since if we are restoring multiple
  -- wildcard matches we want success/failure to be independent.
  for_ pathDatas $ \pd ->
    restoreFn pd `catchAnyCS` \ex -> do
      writeIORef someExRef (Just ex)
      $(logWarn) (T.pack $ displayNoCS ex)
      putStrLn
        $ mconcat
          [ "Error restoring path ",
            show $ pd ^. (#fileName % #unPathI),
            ": ",
            displayNoCS ex
          ]
  readIORef someExRef
  where
    restoreFn' b pt pd = do
      let MkPathI trashPath' = getTrashPath trashHome (pd ^. #fileName)
          MkPathI trashInfoPath' = getTrashInfoPath b trashHome (pd ^. #fileName)

      -- 3. Attempt restore
      PathType.renameFn pt trashPath' (pd ^. #originalPath % #unPathI)

      -- 4. Delete info
      --
      -- NOTE: We do not do any error handling here as at this point we have
      -- accomplished our goal: restore the file. That the trash is now out of sync
      -- is bad, but there isn't anything we can do other than alert the user.
      PW.removeFile trashInfoPath'

findOnePathData ::
  forall m env pd.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m,
    Serialize pd
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  BackendArgs m pd ->
  m PathData
findOnePathData trashHome pathName backendArgs = do
  backend <- asks getBackend
  let trashInfoPath@(MkPathI trashInfoPath') =
        getTrashInfoPath backend trashHome pathName

  pathInfoExists <- doesFileExist trashInfoPath'
  unless pathInfoExists
    $ throwCS
    $ MkTrashEntryNotFoundE pathName trashInfoPath

  contents <- readBinaryFile trashInfoPath'
  pathData <- case decode @pd pathName contents of
    Left err -> throwCS $ MkInfoDecodeE trashInfoPath contents err
    Right pd -> (backendArgs ^. #toCorePathData) trashHome pd

  pathExists <- trashPathExists trashHome pathData
  unless pathExists
    $ throwCS
    $ MkTrashEntryFileNotFoundE trashHome pathName

  pure pathData

findManyPathData ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (NESeq PathData)
findManyPathData backendArgs trashHome pathName = do
  index <- fmap (view _1) . view #unIndex <$> Default.Index.readIndex backendArgs trashHome

  pathNameText <- T.pack <$> decodeOsToFpThrowM (pathName ^. #unPathI)

  matches <- Utils.filterSeqM (pdMatchesWildcard pathNameText) index
  case matches of
    Seq.Empty -> throwCS $ MkTrashEntryWildcardNotFoundE pathName
    pd :<| pds -> pure $ pd :<|| pds
  where
    pdMatchesWildcard pathNameText' pd = do
      fp <- decodeOsToFpThrowM (pd ^. (#fileName % #unPathI))
      pure $ Utils.matchesWildcards pathNameText' (T.pack fp)

-- | Searches for the given trash name in the trash.
findPathData ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasBackend env,
    HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m,
    Serialize pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (NESeq PathData)
findPathData backendArgs trashHome pathName@(MkPathI pathName') = addNamespace "findPathData" $ do
  pathNameStr <- decodeOsToFpThrowM pathName'
  let pathNameTxt = T.pack pathNameStr

  if
    -- 1. Found a (n unescaped) wildcard; findMany (findMany handles the case
    -- where pathName also includes the sequence \\*).
    | hasWildcard pathNameStr -> do
        $(logDebug) $ "Performing wildcard search: '" <> pathNameTxt <> "'"
        findManyPathData backendArgs trashHome pathName
    -- 2. Found the sequence \\*. As we have confirmed there are no unescaped
    -- wildcards by this point, we can simply findOne as normal, after removing
    -- the escape.
    | "\\*" `T.isInfixOf` pathNameTxt -> do
        $(logDebug)
          $ mconcat
            [ "Found escape sequence \\* in path '",
              pathNameTxt,
              "'. Treating as the literal *."
            ]
        let literal = T.replace "\\*" "*" pathNameTxt
        literalPath <- encodeFpToOsThrowM $ T.unpack literal
        NESeq.singleton <$> findOnePathData trashHome (MkPathI literalPath) backendArgs

    -- 3. No * at all; normal
    | otherwise -> do
        $(logDebug) $ "Normal search: '" <> pathNameTxt <> "'"
        NESeq.singleton <$> findOnePathData trashHome pathName backendArgs
  where
    hasWildcard [] = False
    -- escaped; ignore
    hasWildcard ('\\' : '*' : xs) = hasWildcard xs
    hasWildcard ('*' : _) = True
    hasWildcard (_ : xs) = hasWildcard xs

convertBackend ::
  forall m env pdSrc pdDest k.
  ( DecodeExtra pdDest ~ PathI TrashEntryFileName,
    DecodeExtra pdSrc ~ PathI TrashEntryFileName,
    HasCallStack,
    HasTrashHome env,
    Is k A_Getter,
    LabelOptic' "fileName" k pdDest (PathI TrashEntryFileName),
    LabelOptic' "fileName" k pdSrc (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m,
    Serialize pdDest,
    Serialize pdSrc
  ) =>
  BackendArgs m pdSrc ->
  BackendArgs m pdDest ->
  m ()
convertBackend backendArgsSrc backendArgsDest = addNamespace "convertBackend" $ do
  let dest = backendArgsDest ^. #backend
  $(logDebug) $ "Converting backend to: " <> T.pack (Backend.Data.backendName dest)
  trashHome@(MkPathI th) <- asks getTrashHome

  MkIndex index <- Default.Index.readIndex backendArgsSrc trashHome

  let newInfo = th </> [osp|tmp_info_new|]
  PW.createDirectory newInfo

  let fromCorePathData = backendArgsDest ^. #fromCorePathData

  -- NOTE: This is not in-place.

  let createTmp = for_ index $ \(pd, _) -> do
        pd' :: pd <- fromCorePathData pd
        encoded <- encodeThrowM pd'
        let MkPathI fileName = view #fileName pd'
            filePath :: OsPath
            filePath =
              newInfo
                </> fileName
                <.> Backend.Data.backendExt dest

        writeBinaryFile filePath encoded

  -- 1. create converted copy at trash/tmp_info_new
  createTmp `catchAny` \ex -> do
    let msg =
          mconcat
            [ "Exception during conversion:\n",
              displayException ex
            ]
    $(logError) $ T.pack msg
    $(logError) $ "Deleting tmp dir: " <> decodeOsToFpShowText newInfo
    putStrLn msg
    removeDirectoryRecursive newInfo
    throwM ex

  let MkPathI trashInfoDir = Default.Utils.getTrashInfoDir trashHome
      oldInfo = th </> [osp|tmp_info_old|]

  -- 2. Move current trash/info to trash/tmp_info_old
  renameDirectory trashInfoDir oldInfo
    `catchAny` \ex -> do
      let msg =
            mconcat
              [ "Exception moving old trash/info dir:\n",
                displayException ex
              ]
      $(logError) $ T.pack msg
      putStrLn msg
      -- cleanup: remove tmp_info_new
      removeDirectoryRecursive newInfo
      throwM ex

  -- 3. Move trash/tmp_info_new to trash/info
  renameDirectory newInfo trashInfoDir
    `catchAny` \ex -> do
      let msg =
            mconcat
              [ "Exception moving new trash/info dir:\n",
                displayException ex
              ]
      $(logError) $ T.pack msg
      putStrLn msg
      -- cleanup: remove tmp_info_new, move tmp_info_old back
      removeDirectoryRecursive newInfo
      renameDirectory oldInfo trashInfoDir
      throwM ex

  -- 4. Delete trash/tmp_info_old
  removeDirectoryRecursive oldInfo

-- | Merges source into dest, failing if there are any collisions.
mergeTrashDirs ::
  ( HasCallStack,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  -- | src
  PathI TrashHome ->
  -- | dest
  PathI TrashHome ->
  m ()
mergeTrashDirs (MkPathI src) (MkPathI dest) = addNamespace "mergeTrashDirs" $ do
  WDir.copyDirectoryRecursiveConfig config src dest
  $(logDebug) "Merge successful"
  where
    config =
      MkCopyDirConfig
        { overwrite = OverwriteDirectories,
          targetName = TargetNameDest
        }

deleteFileName ::
  ( HasCallStack,
    MonadPathWriter m
  ) =>
  PathI TrashHome ->
  PathData ->
  m ()
deleteFileName trashHome pd = PathType.deleteFn (pd ^. #pathType) trashPath'
  where
    MkPathI trashPath' = getTrashPath trashHome (pd ^. #fileName)

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
trashPathExists ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists th pd = doesPathExist trashPath'
  where
    -- NOTE: doesPathExist rather than doesFile/Dir... as that requires knowing
    -- the path type. See Note [PathData PathType conditions].

    MkPathI trashPath' = getTrashPath th (pd ^. #fileName)

getTrashPath :: PathI TrashHome -> PathI TrashEntryFileName -> PathI TrashEntryPath
getTrashPath trashHome name = trashHome <//> MkPathI pathFiles <//> name

getTrashInfoPath ::
  Backend ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  PathI TrashEntryInfo
getTrashInfoPath backend trashHome name =
  trashHome
    <//> MkPathI pathInfo
    <//> Paths.liftPathI' (<.> Backend.Data.backendExt backend) name
