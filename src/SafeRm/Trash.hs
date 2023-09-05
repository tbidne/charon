{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Interface for the trash directory
module SafeRm.Trash
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
  )
where

import Data.Char qualified as Ch
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effectful.FileSystem.HandleWriter.Dynamic qualified as HW
import Effectful.FileSystem.PathWriter.Dynamic
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteDirectories),
    TargetName (TargetNameDest),
  )
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Effectful.FileSystem.PathWriter.Dynamic qualified as WDir
import Effectful.Terminal.Dynamic qualified as Term
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Index (Index (MkIndex))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType qualified as PathType
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashHome
      ),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize (Serialize (decode), encodeThrowM)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
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
import System.IO qualified as IO

-- | Creates the trash directory if it does not exist.
createTrash ::
  forall env es.
  ( HasCallStack,
    HasTrashHome env,
    PathWriterDynamic :> es,
    Reader env :> es
  ) =>
  Eff es ()
createTrash = do
  trashHome <- asks @env getTrashHome
  let trashPathDir = Env.getTrashPathDir trashHome
      trashInfoDir = Env.getTrashInfoDir trashHome

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
  forall env es.
  ( HasTrashHome env,
    PathReaderDynamic :> es,
    Reader env :> es
  ) =>
  Eff es Bool
doesTrashExist = do
  trashHome <- asks @env getTrashHome
  let MkPathI trashPathDir' = Env.getTrashPathDir trashHome
      MkPathI trashInfoDir' = Env.getTrashInfoDir trashHome

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
          throwM
            $ MkTrashDirInfoNotFoundE trashHome
        -- Info exists; path does not -> Badly formed, throw exception
        (False, True) ->
          throwM
            $ MkTrashDirFilesNotFoundE trashHome

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
mvOriginalToTrash ::
  forall env es.
  ( HasBackend env,
    FileWriterDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es
  ) =>
  PathI TrashHome ->
  Timestamp ->
  PathI TrashEntryOriginalPath ->
  Eff es ()
mvOriginalToTrash trashHome currTime path = addNamespace "mvOriginalToTrash" $ do
  backend <- asks @env getBackend
  (pd, pathType) <- PathData.toPathData backend currTime trashHome path
  $(logDebug) ("Deleting: " <> showt pd)

  let MkPathI trashPath = PathData.pathDataToTrashPath trashHome pd
      MkPathI trashInfoPath = PathData.pathDataToTrashInfoPath trashHome backend pd

  -- 2. Write info file
  --
  -- Perform this before the actual move to be safe i.e. path is only moved
  -- if info is already created.
  encoded <- encodeThrowM pd
  writeBinaryFile trashInfoPath encoded

  -- 4. Move file to trash
  let moveFn = PathType.renameFn pathType (pd ^. #originalPath % #unPathI) trashPath

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
  forall env es.
  ( HasBackend env,
    HasTrashHome env,
    Concurrent :> es,
    FileReaderDynamic :> es,
    HandleWriterDynamic :> es,
    IORefStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    PosixCompatStatic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Bool ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  Eff es (Maybe SomeException)
permDeleteFromTrash force trashHome pathName = addNamespace "permDeleteFromTrash" $ do
  pathDatas <- findPathData @env trashHome pathName
  backend <- asks @env getBackend

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
            noBuffering

            -- NOTE: We normalize the path data as we want to display all fields here.
            pathData' <- PathData.normalizeCore @env pathData
            let pdStr = Utils.renderPretty pathData'
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
    deleteFn pathData `catchAny` \ex -> do
      writeIORef anyExRef (Just ex)
      $(logWarn) (T.pack $ displayException ex)
      putStrLn
        $ mconcat
          [ "Error deleting path ",
            show $ pathData ^. (#fileName % #unPathI),
            ": ",
            displayException ex
          ]
  readIORef anyExRef
  where
    deleteFn' b pd = do
      let MkPathI trashInfoPath' = Env.getTrashInfoPath b trashHome (pd ^. #fileName)

      PathData.deleteFileName trashHome pd
      $(logDebug) ("Deleted: " <> showt pd)
      PW.removeFile trashInfoPath'

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
-- Returns 'True' if any failed. In this case, the error has already been
-- reported, so this is purely for signaling (i.e. should we exit with
-- an error).
restoreTrashToOriginal ::
  forall env es.
  ( HasBackend env,
    FileReaderDynamic :> es,
    IORefStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  Eff es (Maybe SomeException)
restoreTrashToOriginal trashHome pathName = addNamespace "restoreTrashToOriginal" $ do
  -- 1. Get path info
  pathDatas <- findPathData @env trashHome pathName
  backend <- asks @env getBackend

  someExRef <- newIORef Nothing
  let restoreFn pd = do
        let originalPath = pd ^. #originalPath
            fileName = pd ^. #fileName

        $(logDebug) ("Restoring: " <> Paths.toText fileName)

        -- 2. Verify original path is empty
        exists <- PathData.originalPathExists trashHome pd
        when exists
          $ throwM
          $ MkRestoreCollisionE fileName originalPath

        pathType <- PathData.pathDataToType trashHome pd
        restoreFn' backend pathType pd

  -- Need our own error handling here since if we are restoring multiple
  -- wildcard matches we want success/failure to be independent.
  for_ pathDatas $ \pd ->
    restoreFn pd `catchAny` \ex -> do
      writeIORef someExRef (Just ex)
      $(logWarn) (T.pack $ displayException ex)
      putStrLn
        $ mconcat
          [ "Error restoring path ",
            show $ pd ^. (#fileName % #unPathI),
            ": ",
            displayException ex
          ]
  readIORef someExRef
  where
    restoreFn' b pt pd = do
      let MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)
          MkPathI trashInfoPath' = Env.getTrashInfoPath b trashHome (pd ^. #fileName)

      -- 3. Attempt restore
      PathType.renameFn pt trashPath' (pd ^. #originalPath % #unPathI)

      -- 4. Delete info
      --
      -- NOTE: We do not do any error handling here as at this point we have
      -- accomplished our goal: restore the file. That the trash is now out of sync
      -- is bad, but there isn't anything we can do other than alert the user.
      PW.removeFile trashInfoPath'

findOnePathData ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    FileReaderDynamic :> es,
    PathReaderDynamic :> es,
    Reader env :> es
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  Eff es PathData
findOnePathData trashHome pathName = do
  backend <- asks @env getBackend
  let trashInfoPath@(MkPathI trashInfoPath') =
        Env.getTrashInfoPath backend trashHome pathName

  pathInfoExists <- doesFileExist trashInfoPath'
  unless pathInfoExists
    $ throwM
    $ MkTrashEntryNotFoundE pathName trashInfoPath

  contents <- readBinaryFile trashInfoPath'
  pathData <- case decode (backend, pathName) contents of
    Left err -> throwM $ MkInfoDecodeE trashInfoPath contents err
    Right pd -> pure pd

  pathExists <- PathData.trashPathExists trashHome pathData
  unless pathExists
    $ throwM
    $ MkTrashEntryFileNotFoundE trashHome pathName

  pure pathData

findManyPathData ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    FileReaderDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    Reader env :> es
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  Eff es (NESeq PathData)
findManyPathData trashHome pathName = do
  MkIndex index <- Index.readIndex @env trashHome

  pathNameText <- T.pack <$> decodeOsToFpThrowM (pathName ^. #unPathI)

  matches <- Utils.filterSeqM (pdMatchesWildcard pathNameText) index
  case matches of
    Seq.Empty -> throwM $ MkTrashEntryWildcardNotFoundE pathName
    pd :<| pds -> pure $ pd :<|| pds
  where
    pdMatchesWildcard pathNameText' pd = do
      fp <- decodeOsToFpThrowM (pd ^. (#fileName % #unPathI))
      pure $ Utils.matchesWildcards pathNameText' (T.pack fp)

noBuffering :: (HandleWriterDynamic :> es) => Eff es ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = HW.hSetBuffering h NoBuffering

-- | Searches for the given trash name in the trash.
findPathData ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    FileReaderDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    Reader env :> es
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  Eff es (NESeq PathData)
findPathData trashHome pathName@(MkPathI pathName') = addNamespace "findPathData" $ do
  pathNameStr <- decodeOsToFpThrowM pathName'
  let pathNameTxt = T.pack pathNameStr

  if
    -- 1. Found a (n unescaped) wildcard; findMany (findMany handles the case
    -- where pathName also includes the sequence \\*).
    | hasWildcard pathNameStr -> do
        $(logDebug) $ "Performing wildcard search: '" <> pathNameTxt <> "'"
        findManyPathData @env trashHome pathName
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
        NESeq.singleton <$> findOnePathData @env trashHome (MkPathI literalPath)

    -- 3. No * at all; normal
    | otherwise -> do
        $(logDebug) $ "Normal search: '" <> pathNameTxt <> "'"
        NESeq.singleton <$> findOnePathData @env trashHome pathName
  where
    hasWildcard [] = False
    -- escaped; ignore
    hasWildcard ('\\' : '*' : xs) = hasWildcard xs
    hasWildcard ('*' : _) = True
    hasWildcard (_ : xs) = hasWildcard xs

convertBackend ::
  forall env es.
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    FileReaderDynamic :> es,
    FileWriterDynamic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Backend ->
  Eff es ()
convertBackend dest = addNamespace "convertBackend" $ do
  $(logDebug) $ "Converting backend to: " <> T.pack (Backend.backendArg dest)
  trashHome@(MkPathI th) <- asks @env getTrashHome
  MkIndex index <- Index.readIndex @env trashHome

  let newInfo = th </> [osp|tmp_info_new|]
  PW.createDirectory newInfo

  -- NOTE: This is not in-place.

  let createTmp = for_ index $ \pd -> do
        let pd' = PathData.convert pd dest
        encoded <- encodeThrowM pd'
        let filePath =
              newInfo
                </> (pd' ^. (#fileName % #unPathI))
                <> Env.trashInfoExtensionOsPath dest

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

  let MkPathI trashInfoDir = Env.getTrashInfoDir trashHome
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
  ( LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PathReaderDynamic :> es,
    PathWriterDynamic :> es
  ) =>
  -- | src
  PathI TrashHome ->
  -- | dest
  PathI TrashHome ->
  Eff es ()
mergeTrashDirs (MkPathI src) (MkPathI dest) = addNamespace "mergeTrashDirs" $ do
  WDir.copyDirectoryRecursiveConfig config src dest
  $(logDebug) "Merge successful"
  where
    config =
      MkCopyDirConfig
        { overwrite = OverwriteDirectories,
          targetName = TargetNameDest
        }
