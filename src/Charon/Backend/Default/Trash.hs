{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interface for the default trash directory
module Charon.Backend.Default.Trash
  ( -- * Trash directory
    createTrash,
    createTrashDir,
    doesTrashExist,
    doesTrashExistPath,

    -- * Main actions
    mvOriginalToTrash,
    mvOriginalToTrash_,
    restoreTrashToOriginal,
    permDeleteFromTrash,

    -- * Transformations
    mergeTrashDirs,

    -- * Utils
    PathDataSearchResult (..),
    findPathData,
    getTrashPath,
    mvPdToTrash,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend.Data
import Charon.Backend.Default.BackendArgs (BackendArgs)
import Charon.Backend.Default.Exception
  ( TrashDirFilesNotFoundE (MkTrashDirFilesNotFoundE),
    TrashDirInfoNotFoundE (MkTrashDirInfoNotFoundE),
  )
import Charon.Backend.Default.Index qualified as Default.Index
import Charon.Backend.Default.Utils qualified as Default.Utils
import Charon.Class.Serial (Serial (DecodeExtra, decode), encodeThrowM)
import Charon.Data.PathData (PathData)
import Charon.Data.PathData qualified as PathData.Core
import Charon.Data.PathType (PathTypeW)
import Charon.Data.PathType qualified as PathType
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashDirFiles,
        TrashDirInfo,
        TrashEntryFileName,
        TrashEntryInfo,
        TrashEntryOriginalPath,
        TrashEntryPath,
        TrashHome
      ),
    (<//>),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.Timestamp (Timestamp)
import Charon.Env (HasTrashHome (getTrashHome))
import Charon.Exception
  ( InfoDecodeE (MkInfoDecodeE),
    RestoreCollisionE (MkRestoreCollisionE),
    TrashEntryFileNotFoundE (MkTrashEntryFileNotFoundE),
    TrashEntryNotFoundE (MkTrashEntryNotFoundE),
    TrashEntryWildcardNotFoundE (MkTrashEntryWildcardNotFoundE),
  )
import Charon.Prelude
import Charon.Runner.Phase (Force, Prompt)
import Charon.Utils qualified as Utils
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteDirectories),
    TargetName (TargetNameDest),
  )
import Effects.FileSystem.PathWriter qualified as PW
import Effects.FileSystem.PathWriter qualified as WDir
import FileSystem.OsPath qualified as OsPath

-- | Creates the trash directory if it does not exist.
createTrash ::
  ( HasCallStack,
    HasTrashHome env,
    MonadLoggerNS m env k,
    MonadPathWriter m,
    MonadReader env m
  ) =>
  m (PathI TrashDirFiles, PathI TrashDirInfo)
createTrash = addNamespace "createTrash" $ asks getTrashHome >>= createTrashDir

-- | Creates the trash directory if it does not exist.
createTrashDir ::
  ( HasCallStack,
    MonadLoggerNS m env k,
    MonadPathWriter m
  ) =>
  PathI TrashHome ->
  m (PathI TrashDirFiles, PathI TrashDirInfo)
createTrashDir trashHome = addNamespace "createTrashDir" $ do
  $(logDebug) "Creating trash if it does not exist"

  let trashPathDir = Default.Utils.getTrashPathDir trashHome
      trashInfoDir = Default.Utils.getTrashInfoDir trashHome

  Paths.applyPathI (createDirectoryIfMissing False) trashHome
  Paths.applyPathI (createDirectoryIfMissing False) trashPathDir
  Paths.applyPathI (createDirectoryIfMissing False) trashInfoDir

  pure (trashPathDir, trashInfoDir)

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
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Bool
doesTrashExist = addNamespace "doesTrashExist" $ asks getTrashHome >>= doesTrashExistPath

-- | Returns 'False' if @\<trash-home\>@ does not exist. If @\<trash-home\>@
-- /does/ exist but is "badly-formed" i.e. one of
--
-- * \<trash-home\>/files
-- * \<trash-home\>/info
--
-- does not, throws 'TrashDirFilesNotFoundE' or 'TrashDirInfoNotFoundE'.
--
-- If all three dirs exist, returns 'True'.
doesTrashExistPath ::
  ( HasCallStack,
    MonadLoggerNS m env k,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  m Bool
doesTrashExistPath trashHome = addNamespace "doesTrashExistPath" $ do
  let MkPathI trashPathDir' = Default.Utils.getTrashPathDir trashHome
      MkPathI trashInfoDir' = Default.Utils.getTrashInfoDir trashHome

  homeExists <- doesDirectoryExist (trashHome ^. #unPathI)
  if not homeExists
    then do
      $(logDebug) "Trash does not exist"
      pure False
    else do
      pathExists <- doesDirectoryExist trashPathDir'
      infoExists <- doesDirectoryExist trashInfoDir'

      case (pathExists, infoExists) of
        -- Everything exists -> true
        (True, True) -> do
          $(logDebug) "Trash exists"
          pure True
        -- Info and Path both do not exist -> false
        (False, False) -> do
          $(logDebug) "Trash/ exists but info/ and files/ do not"
          pure False
        -- Path exists; info does not -> Badly formed, throw exception
        (True, False) -> do
          $(logError) "Trash/files exists but info/ does not"
          throwM
            $ MkTrashDirInfoNotFoundE trashHome
        -- Info exists; path does not -> Badly formed, throw exception
        (False, True) -> do
          $(logError) "Trash/info exists but files/ does not"
          throwM
            $ MkTrashDirFilesNotFoundE trashHome

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
mvOriginalToTrash_ ::
  ( HasCallStack,
    Is k A_Getter,
    LabelOptic' "fileName" k pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k pd (PathI TrashEntryOriginalPath),
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m env k,
    MonadPathWriter m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  Timestamp ->
  PathI TrashEntryOriginalPath ->
  m ()
mvOriginalToTrash_ backendArgs th ts =
  addNamespace "mvOriginalToTrash_"
    . void
    . mvOriginalToTrash backendArgs th ts

-- | Moves the 'PathData'\'s @originalPath@ to the trash. Returns the
-- created pd.
mvOriginalToTrash ::
  ( HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k1 pd (PathI TrashEntryOriginalPath),
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m env k2,
    MonadPathWriter m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  Timestamp ->
  PathI TrashEntryOriginalPath ->
  m (pd, PathTypeW, PathI TrashEntryPath)
mvOriginalToTrash
  backendArgs
  trashHome
  currTime
  path = addNamespace "mvOriginalToTrash" $ do
    $(logDebug) $ "Path: " <> Paths.toText path
    -- 1. Derive core path data fields.
    let backend = backendArgs ^. #backend
    (pd, pathType) <- (backendArgs ^. #makePathData) currTime trashHome path

    trashPathI <- mvPdToTrash backend trashHome pd pathType
    pure (pd, pathType, trashPathI)

-- | Internal function used by 'mvOriginalToTrash'.
mvPdToTrash ::
  ( HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    LabelOptic' "originalPath" k1 pd (PathI TrashEntryOriginalPath),
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m env k2,
    MonadPathWriter m,
    Serial pd,
    Show pd
  ) =>
  Backend ->
  PathI TrashHome ->
  pd ->
  PathTypeW ->
  m (PathI TrashEntryPath)
mvPdToTrash backend trashHome pd pathType = do
  $(logDebug) ("Deleting: " <> showt pd <> ", " <> showt pathType)

  let fileName = pd ^. #fileName
      trashPathI@(MkPathI trashPath) = getTrashPath trashHome fileName
      MkPathI trashInfoPath = getTrashInfoPath backend trashHome fileName

  -- 2. Write info file
  --
  -- Perform this before the actual move to be safe i.e. path is only moved
  -- if info is already created.
  encoded <- encodeThrowM pd
  writeBinaryFile trashInfoPath encoded

  $(logDebug) ("Wrote to file: " <> showt encoded)

  -- 4. Move file to trash
  let MkPathI opath = pd ^. #originalPath
      moveFn = PathType.renameFn pathType opath trashPath

  -- 5. If move failed, roll back info file
  moveFn `catchSync` \ex -> do
    $(logError) ("Error moving file to trash: " <> Utils.displayExT ex)
    PW.removeFile trashInfoPath
    throwM ex

  $(logInfo) ("Moved to trash: " <> showt pd)

  pure trashPathI

-- | Permanently deletes the trash path. Returns 'True' if any deletes fail.
-- In this case, the error has already been reported, so this is purely for
-- signaling (i.e. should we exit with an error).
permDeleteFromTrash ::
  forall m pd env k1 k2.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadAsync m,
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
  Prompt ->
  IORef (Seq (PathI TrashEntryFileName)) ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m ()
permDeleteFromTrash
  backendArgs
  postHook
  prompt
  deletedPathsRef
  trashHome
  pathName = addNamespace "permDeleteFromTrash" $ do
    $(logDebug) $ "Path: " <> Paths.toText pathName
    pathDatas <-
      findPathData backendArgs trashHome pathName >>= \case
        SearchSuccess pds -> do
          $(logDebug) $ "Found path data: " <> showt pds
          pure pds
        SearchSingleFailure path -> do
          $(logError) $ "Single search failed: " <> Paths.toText path
          throwM $ MkTrashEntryNotFoundE path
        SearchWildcardFailure path -> do
          $(logError) $ "Wildcard search failed: " <> Paths.toText path
          throwM $ MkTrashEntryWildcardNotFoundE path

    let backend = backendArgs ^. #backend
        deleteFn :: PathData -> m ()
        deleteFn pathData = do
          $(logDebug) ("Deleting: " <> Paths.toText (pathData ^. #fileName))
          if prompt ^. #unPrompt
            then do
              let pdStr = Utils.renderPretty pathData
              putTextLn $ "\n" <> pdStr
              ans <- Utils.askYesNoQ "\nPermanently delete"
              if ans
                then void $ deleteFn' backend pathData
                else pure ()
            else
              -- NOTE: Technically don't need the pathdata if noPrompt is on, since we have
              -- the path and can just delete it. Nevertheless, we retrieve the pathData
              -- so that noPrompt does not change the semantics i.e. can only delete
              -- "well-behaved" files, and we don't have to do a redundant file/directory
              -- check.
              deleteFn' backend pathData

    -- Need our own error handling here since if we are deleting multiple
    -- wildcard matches we want success/failure to be independent.
    for_ pathDatas $ \pathData -> do
      deleteFn pathData
      postHook pathData
    where
      deleteFn' b pd = do
        let MkPathI trashInfoPath' = getTrashInfoPath b trashHome (pd ^. #fileName)

        deleteFileName trashHome pd
        modifyIORef' deletedPathsRef (:|> pd ^. #fileName)
        $(logInfo) ("Permanently deleted: " <> showt pd)
        PW.removeFile trashInfoPath'

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
-- Returns 'True' if any failed. In this case, the error has already been
-- reported, so this is purely for signaling (i.e. should we exit with
-- an error).
restoreTrashToOriginal ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
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
  Prompt ->
  IORef (Seq (PathI TrashEntryOriginalPath)) ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m ()
restoreTrashToOriginal
  backendArgs
  postHook
  force
  prompt
  restoredPathsRef
  trashHome
  pathName = addNamespace "restoreTrashToOriginal" $ do
    $(logDebug) $ "Path: " <> Paths.toText pathName
    -- 1. Get path info
    pathDatas <-
      findPathData backendArgs trashHome pathName >>= \case
        SearchSuccess pds -> pure pds
        SearchSingleFailure path -> throwM $ MkTrashEntryNotFoundE path
        SearchWildcardFailure path -> throwM $ MkTrashEntryWildcardNotFoundE path

    let restoreFn pd = do
          let fileName = pd ^. #fileName

          $(logDebug) ("Restoring: " <> Paths.toText fileName)

          if prompt ^. #unPrompt
            then restorePrompt pd
            else restoreNoPrompt pd

    -- Need our own error handling here since if we are restoring multiple
    -- wildcard matches we want success/failure to be independent.
    for_ pathDatas $ \pathData -> do
      restoreFn pathData
      postHook pathData
    where
      backend = backendArgs ^. #backend

      -- NOTE: The restore(prompt|noPrompt) functions potentially ask the
      -- user to confirm. The overwrite(Ask|Force|Throw) functions handle
      -- collisions and call the actual restore function.

      -- 1. No --no-prompt. Ask before restoring paths.
      restorePrompt pd = do
        let pdStr = Utils.renderPretty pd

        putTextLn pdStr

        ans <- Utils.askYesNoQ "\nRestore"

        if ans
          then
            if force ^. #unForce
              then
                -- 2.1. --force: Do not ask for overwrites.
                overwriteForce pd
              else
                -- 2.2. No --force: Ask for overwrites.
                overwriteAsk pd
          else pure ()

      -- 2. --no-prompt: Do not prompt before restoring, or before overwrites.
      restoreNoPrompt pd = do
        if force ^. #unForce
          then
            -- 2.1. --force: overwrite paths without asking.
            overwriteForce pd
          else
            -- 2.2. No --force: throw on overwrites.
            overwriteThrow pd

      -- Asks the user if we want to overwrite collisions.
      overwriteAsk pd = do
        let pathType = pd ^. #pathType
        exists <- PathData.Core.originalPathExists pd
        if exists
          then do
            ans <- Utils.askYesNoQ "\nPath exists at original. Overwrite"
            if ans
              then restoreFn' backend pathType pd
              else pure ()
          else restoreFn' backend pathType pd

      -- Collisions throw exception.
      overwriteThrow pd = do
        let originalPath = pd ^. #originalPath
            fileName = pd ^. #fileName
        exists <- PathData.Core.originalPathExists pd
        when exists
          $ throwM
          $ MkRestoreCollisionE fileName originalPath
        restoreFn' backend (pd ^. #pathType) pd

      -- Overwrites forcibly.
      overwriteForce pd = do
        exists <- PathData.Core.originalPathExists pd
        when exists $ PathType.deleteFn (pd ^. #pathType) (pd ^. #originalPath % #unPathI)
        restoreFn' backend (pd ^. #pathType) pd

      -- Low level restore function.
      restoreFn' b pt pd = do
        let MkPathI trashPath' = getTrashPath trashHome (pd ^. #fileName)
            MkPathI trashInfoPath' = getTrashInfoPath b trashHome (pd ^. #fileName)

        -- 3. Attempt restore
        let original = pd ^. #originalPath % #unPathI
        PathType.renameFn pt trashPath' original
        modifyIORef' restoredPathsRef (:|> pd ^. #originalPath)
        $(logInfo) $ "Restored: " <> decodeDisplayExT original

        -- 4. Delete info
        --
        -- NOTE: We do not do any error handling here as at this point we have
        -- accomplished our goal: restore the file. That the trash is now out of sync
        -- is bad, but there isn't anything we can do other than alert the user.
        PW.removeFile trashInfoPath'

-- | Searches for a single result. Throws exceptions for decode errors or if
-- the info file exists, yet the path itself does not.
findOnePathData ::
  forall m pd env k.
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k,
    MonadPathReader m,
    Serial pd,
    Show pd
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  BackendArgs m pd ->
  m (Maybe PathData)
findOnePathData trashHome pathName backendArgs = addNamespace "findOnePathData" $ do
  $(logDebug) $ "Searching for: " <> Paths.toText pathName
  let backend = backendArgs ^. #backend
      trashInfoPath@(MkPathI trashInfoPath') =
        getTrashInfoPath backend trashHome pathName

  pathInfoExists <- doesFileExist trashInfoPath'
  if not pathInfoExists
    then do
      $(logDebug) $ "File does not exist: " <> Paths.toText trashInfoPath
      pure Nothing
    else do
      contents <- readBinaryFile trashInfoPath'
      pathData <- case decode @pd pathName contents of
        Left err -> do
          $(logError) $ "Decode error: " <> showt contents
          throwM $ MkInfoDecodeE trashInfoPath contents err
        Right pd -> do
          $(logDebug) $ "Search successful: " <> showt pd
          (backendArgs ^. #toCorePathData) trashHome pd

      -- if we get here then we know the trash path info exists, so the path
      -- itself better exist.
      pathExists <- trashPathExists trashHome pathData
      unless pathExists $ do
        $(logError) "Path does not exist"
        throwM $ MkTrashEntryFileNotFoundE trashHome pathName

      pure $ Just pathData

-- | Performs a wildcard search. Can throw exceptions if paths fail to decode,
-- or if the index fails to read (i.e. anything malformed).
findManyPathData ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k2,
    MonadPathReader m,
    Serial pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (Seq PathData)
findManyPathData
  backendArgs
  trashHome
  pathName = addNamespace "findManyPathData" $ do
    $(logDebug) $ "Searching for: " <> Paths.toText pathName
    index <- fmap (view _1) . view #unIndex <$> Default.Index.readIndexTrashHome backendArgs trashHome
    $(logDebug) $ "Index: " <> showt index

    pathNameText <- T.pack <$> OsPath.decodeThrowM (pathName ^. #unPathI)

    Utils.filterSeqM (pdMatchesWildcard pathNameText) index
    where
      pdMatchesWildcard pathNameText' pd = do
        fp <- OsPath.decodeThrowM (pd ^. (#fileName % #unPathI))
        let fpTxt = T.pack fp
            matches = Utils.matchesWildcards pathNameText' fpTxt

        when matches
          $ $(logDebug)
          $ "Found a match: "
          <> showt pd

        pure matches

-- | The result of searching for a trash entry.
data PathDataSearchResult
  = SearchSuccess (NESeq PathData)
  | SearchSingleFailure (PathI TrashEntryFileName)
  | SearchWildcardFailure (PathI TrashEntryFileName)
  deriving stock (Eq, Show)

-- | Searches for the given trash name in the trash.
findPathData ::
  ( DecodeExtra pd ~ PathI TrashEntryFileName,
    HasCallStack,
    Is k1 A_Getter,
    LabelOptic' "fileName" k1 pd (PathI TrashEntryFileName),
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNS m env k2,
    MonadPathReader m,
    Serial pd,
    Show pd
  ) =>
  BackendArgs m pd ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m PathDataSearchResult
findPathData
  backendArgs
  trashHome
  pathName@(MkPathI pathName') = addNamespace "findPathData" $ do
    $(logDebug) $ "Searching for: " <> Paths.toText pathName

    pathNameStr <- OsPath.decodeThrowM pathName'
    let pathNameTxt = T.pack pathNameStr

    if
      -- 1. Found a (n unescaped) wildcard; findMany (findMany handles the case
      -- where pathName also includes the sequence \\*).
      | hasWildcard pathNameStr -> do
          findManyPathData backendArgs trashHome pathName <&> \case
            Seq.Empty -> SearchWildcardFailure pathName
            (x :<| xs) -> SearchSuccess (x :<|| xs)
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
          literalPath <- OsPath.encodeValidThrowM $ T.unpack literal
          findOnePathData trashHome (MkPathI literalPath) backendArgs <&> \case
            Nothing -> SearchSingleFailure pathName
            Just pd -> SearchSuccess (pd :<|| Seq.empty)

      -- 3. No * at all; normal
      | otherwise -> do
          findOnePathData trashHome pathName backendArgs <&> \case
            Nothing -> SearchSingleFailure pathName
            Just pd -> SearchSuccess (pd :<|| Seq.empty)
    where
      hasWildcard [] = False
      -- escaped; ignore
      hasWildcard ('\\' : '*' : xs) = hasWildcard xs
      hasWildcard ('*' : _) = True
      hasWildcard (_ : xs) = hasWildcard xs

-- | Merges source into dest, failing if there are any collisions.
mergeTrashDirs ::
  ( HasCallStack,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m env k,
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
  $(logInfo) "Merge successful"
  where
    config =
      MkCopyDirConfig
        { overwrite = OverwriteDirectories,
          targetName = TargetNameDest
        }

deleteFileName ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
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
    MonadCatch m,
    MonadPathReader m
  ) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists th pd = doesAnyPathExist trashPath'
  where
    -- NOTE: doesPathExist rather than doesFile/Dir... as that requires knowing
    -- the path type. See Note [PathData PathType conditions].

    MkPathI trashPath' = getTrashPath th (pd ^. #fileName)

getTrashPath :: PathI TrashHome -> PathI TrashEntryFileName -> PathI TrashEntryPath
getTrashPath trashHome name = trashHome <//> MkPathI Default.Utils.pathFiles <//> name

getTrashInfoPath ::
  Backend ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  PathI TrashEntryInfo
getTrashInfoPath backend trashHome name =
  trashHome
    <//> MkPathI Default.Utils.pathInfo
    <//> Paths.liftPathI' (<.> Backend.Data.backendExt backend) name
