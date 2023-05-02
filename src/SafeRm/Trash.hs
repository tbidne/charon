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
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (..),
    MonadPathWriter (..),
    Overwrite (..),
    TargetName (..),
  )
import Effects.FileSystem.PathWriter qualified as WDir
import Effects.System.Terminal (MonadTerminal (..))
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.Index (Index (..))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType qualified as PathType
import SafeRm.Data.Paths (PathI (..), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env (HasBackend (getBackend), HasTrashHome (..))
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( InfoDecodeE (..),
    RestoreCollisionE (MkRestoreCollisionE),
    TrashDirFilesNotFoundE (..),
    TrashDirInfoNotFoundE (..),
    TrashEntryFileNotFoundE (MkTrashEntryFileNotFoundE),
    TrashEntryNotFoundE (MkTrashEntryNotFoundE),
    TrashEntryWildcardNotFoundE (..),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils
import System.IO qualified as IO

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
  ( HasCallStack,
    HasTrashHome env,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  m Bool
doesTrashExist = do
  trashHome <- asks getTrashHome
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
          throwCS $
            MkTrashDirInfoNotFoundE trashHome
        -- Info exists; path does not -> Badly formed, throw exception
        (False, True) ->
          throwCS $
            MkTrashDirFilesNotFoundE trashHome

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
mvOriginalToTrash ::
  ( HasBackend env,
    HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m
  ) =>
  PathI TrashHome ->
  Timestamp ->
  PathI TrashEntryOriginalPath ->
  m ()
mvOriginalToTrash trashHome currTime path = addNamespace "mvOriginalToTrash" $ do
  backend <- asks getBackend
  (pd, pathType) <- PathData.toPathData backend currTime trashHome path
  $(logDebug) ("Deleting: " <> showt pd)

  let MkPathI trashPath = PathData.pathDataToTrashPath trashHome pd
      MkPathI trashInfoPath = PathData.pathDataToTrashInfoPath trashHome backend pd

  -- 2. Write info file
  --
  -- Perform this before the actual move to be safe i.e. path is only moved
  -- if info is already created.
  let encoded = encode pd
  writeBinaryFile trashInfoPath encoded

  -- 4. Move file to trash
  let moveFn = PathType.renameFn pathType (pd ^. #originalPath % #unPathI) trashPath

  -- 5. If move failed, roll back info file
  moveFn `catchAny` \ex -> do
    $(logError) ("Error moving file to trash: " <> displayExceptiont ex)
    removeFile trashInfoPath
    throwM ex

  $(logDebug) ("Moved to trash: " <> showt pd)

-- | Permanently deletes the trash path. Returns 'True' if any deletes fail.
-- In this case, the error has already been reported, so this is purely for
-- signaling (i.e. should we exit with an error).
permDeleteFromTrash ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Bool ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (Maybe SomeException)
permDeleteFromTrash force trashHome pathName = addNamespace "permDeleteFromTrash" $ do
  pathDatas <- findPathData trashHome pathName
  backend <- asks getBackend

  anyExRef <- newIORef Nothing
  let deleteFn pathData = do
        $(logDebug) ("Deleting: " <> T.pack (pathData ^. (#fileName % #unPathI)))
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
            pathData' <- PathData.normalizeCore pathData
            let pdStr = Utils.renderPretty pathData'
            putTextLn pdStr
            putStr "\nPermanently delete (y/n)? "
            c <- Ch.toLower <$> getChar
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
      putStrLn $
        mconcat
          [ "Error deleting path '",
            pathData ^. (#fileName % #unPathI),
            "': ",
            displayNoCS ex
          ]
  readIORef anyExRef
  where
    deleteFn' b pd = do
      let MkPathI trashInfoPath' = Env.getTrashInfoPath b trashHome (pd ^. #fileName)

      PathData.deleteFileName trashHome pd
      $(logDebug) ("Deleted: " <> showt pd)
      removeFile trashInfoPath'

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
-- Returns 'True' if any failed. In this case, the error has already been
-- reported, so this is purely for signaling (i.e. should we exit with
-- an error).
restoreTrashToOriginal ::
  ( HasBackend env,
    HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (Maybe SomeException)
restoreTrashToOriginal trashHome pathName = addNamespace "restoreTrashToOriginal" $ do
  -- 1. Get path info
  pathDatas <- findPathData trashHome pathName
  backend <- asks getBackend

  someExRef <- newIORef Nothing
  let restoreFn pd = do
        let originalPath = pd ^. #originalPath
            fileName = pd ^. #fileName

        $(logDebug) ("Restoring: " <> T.pack (fileName ^. #unPathI))

        -- 2. Verify original path is empty
        exists <- PathData.originalPathExists trashHome pd
        when exists $
          throwCS $
            MkRestoreCollisionE fileName originalPath

        pathType <- PathData.pathDataToType trashHome pd
        restoreFn' backend pathType pd

  -- Need our own error handling here since if we are restoring multiple
  -- wildcard matches we want success/failure to be independent.
  for_ pathDatas $ \pd ->
    restoreFn pd `catchAnyCS` \ex -> do
      writeIORef someExRef (Just ex)
      $(logWarn) (T.pack $ displayNoCS ex)
      putStrLn $
        mconcat
          [ "Error restoring path '",
            pd ^. (#fileName % #unPathI),
            "': ",
            displayNoCS ex
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
      removeFile trashInfoPath'

findOnePathData ::
  ( HasBackend env,
    HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m PathData
findOnePathData trashHome pathName = do
  backend <- asks getBackend
  let trashInfoPath@(MkPathI trashInfoPath') =
        Env.getTrashInfoPath backend trashHome pathName

  pathInfoExists <- doesFileExist trashInfoPath'
  unless pathInfoExists $
    throwCS $
      MkTrashEntryNotFoundE pathName trashInfoPath

  contents <- readBinaryFile trashInfoPath'
  pathData <- case decode (backend, pathName) contents of
    Left err -> throwCS $ MkInfoDecodeE trashInfoPath contents err
    Right pd -> pure pd

  pathExists <- PathData.trashPathExists trashHome pathData
  unless pathExists $
    throwCS $
      MkTrashEntryFileNotFoundE trashHome pathName

  pure pathData

findManyPathData ::
  ( HasBackend env,
    HasCallStack,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (NESeq PathData)
findManyPathData trashHome pathName = do
  MkIndex index <- Index.readIndex trashHome

  case Seq.filter pdMatchesWildcard index of
    Seq.Empty -> throwCS $ MkTrashEntryWildcardNotFoundE pathName
    pd :<| pds -> pure $ pd :<|| pds
  where
    pdMatchesWildcard =
      Utils.matchesWildcards pathNameText
        . T.pack
        . view (#fileName % #unPathI)

    pathNameText = T.pack $ pathName ^. #unPathI

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = hSetBuffering h NoBuffering

-- | Searches for the given trash name in the trash.
findPathData ::
  ( HasBackend env,
    HasCallStack,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadReader env m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (NESeq PathData)
findPathData trashHome pathName@(MkPathI pathName') = addNamespace "findPathData" $ do
  if
      -- 1. Found a (n unescaped) wildcard; findMany (findMany handles the case
      -- where pathName also includes the sequence \\*).
      | hasWildcard pathName' -> do
          $(logDebug) $ "Performing wildcard search: '" <> pathNameTxt <> "'"
          findManyPathData trashHome pathName
      -- 2. Found the sequence \\*. As we have confirmed there are no unescaped
      -- wildcards by this point, we can simply findOne as normal, after removing
      -- the escape.
      | "\\*" `T.isInfixOf` pathNameTxt -> do
          $(logDebug) $
            mconcat
              [ "Found escape sequence \\* in path '",
                pathNameTxt,
                "'. Treating as the literal *."
              ]
          let literal = T.replace "\\*" "*" pathNameTxt
          NESeq.singleton <$> findOnePathData trashHome (MkPathI $ T.unpack literal)

      -- 3. No * at all; normal
      | otherwise -> do
          $(logDebug) $ "Normal search: '" <> pathNameTxt <> "'"
          NESeq.singleton <$> findOnePathData trashHome pathName
  where
    pathNameTxt = T.pack pathName'

    hasWildcard [] = False
    -- escaped; ignore
    hasWildcard ('\\' : '*' : xs) = hasWildcard xs
    hasWildcard ('*' : _) = True
    hasWildcard (_ : xs) = hasWildcard xs

convertBackend ::
  ( HasBackend env,
    HasCallStack,
    HasTrashHome env,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  Backend ->
  m ()
convertBackend dest = addNamespace "convertBackend" $ do
  $(logDebug) $ "Converting backend to: " <> T.pack (Backend.backendArg dest)
  trashHome@(MkPathI th) <- asks getTrashHome
  MkIndex index <- Index.readIndex trashHome

  let newInfo = th </> "tmp_info_new"
  createDirectory newInfo

  -- NOTE: This is not in-place.

  let createTmp = for_ index $ \pd -> do
        let pd' = PathData.convert pd dest
            encoded = encode pd'
            filePath = newInfo </> (pd' ^. (#fileName % #unPathI)) <> Env.trashInfoExtension dest

        writeBinaryFile filePath encoded

  -- 1. create converted copy at trash/tmp_info_new
  createTmp `catchAny` \ex -> do
    let msg =
          mconcat
            [ "Exception during conversion:\n",
              displayException ex
            ]
    $(logError) $ T.pack msg
    $(logError) $ "Deleting tmp dir: " <> T.pack newInfo
    putStrLn msg
    removeDirectoryRecursive newInfo
    throwM ex

  let MkPathI trashInfoDir = Env.getTrashInfoDir trashHome
      oldInfo = th </> "tmp_info_old"

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
        { overwrite = OverwriteTarget,
          targetName = TargetNameDest
        }
