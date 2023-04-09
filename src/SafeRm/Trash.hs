{-# LANGUAGE TemplateHaskell #-}

-- | Interface for the trash directory
--
-- @since 0.1
module SafeRm.Trash
  ( -- * Trash directory
    createTrash,
    doesTrashExist,

    -- * Main actions
    mvOriginalToTrash,
    restoreTrashToOriginal,
    permDeleteFromTrash,
  )
where

import Data.Char qualified as Ch
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.Exception (onException)
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.FileSystem.PathWriter (removeFile)
import Effects.System.Terminal (MonadTerminal (..))
import SafeRm.Data.Index (Index (..))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env (HasTrashHome (..))
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( InfoDecodeE (..),
    RestoreCollisionE (MkRestoreCollisionE),
    TrashDirInfoNotFoundE (..),
    TrashDirPathsNotFoundE (..),
    TrashEntryNotFoundE (MkTrashEntryNotFoundE),
    TrashEntryPathNotFoundE (MkTrashEntryPathNotFoundE),
    TrashEntryWildcardNotFoundE (..),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as Utils
import System.IO qualified as IO

-- | Creates the trash directory if it does not exist.
--
-- @since 0.1
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
-- * \<trash-home\>/paths
-- * \<trash-home\>/info
--
-- does not, throws 'TrashDirPathsNotFoundE' or 'TrashDirInfoNotFoundE'.
--
-- If all three dirs exist, returns 'True'.
--
-- @since 0.1
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
            MkTrashDirPathsNotFoundE trashHome

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
--
-- @since 0.1
mvOriginalToTrash ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  Timestamp ->
  PathI TrashEntryOriginalPath ->
  m ()
mvOriginalToTrash trashHome currTime path = addNamespace "mvOriginalToTrash" $ do
  -- 1. Transform path to PathData
  pd <- PathData.toPathData currTime trashHome path
  $(logDebug) ("Deleting: " <> showt pd)

  let MkPathI trashPath = PathData.pathDataToTrashPath trashHome pd
      MkPathI trashInfoPath = PathData.pathDataToTrashInfoPath trashHome pd

  -- 2. Write info file
  --
  -- Perform this before the actual move to be safe i.e. path is only moved
  -- if info is already created.
  let encoded = PathData.encode pd
  writeBinaryFile trashInfoPath encoded

  -- 4. Move file to trash
  let moveFn = mvPathData pd (pd ^. #originalPath % #unPathI) trashPath

  -- 5. If move failed, roll back info file
  moveFn `onException` removeFile trashInfoPath
  $(logDebug) ("Moved to trash: " <> showt pd)

-- | Permanently deletes the trash path. Returns 'True' if any deletes fail.
-- In this case, the error has already been reported, so this is purely for
-- signaling (i.e. should we exit with an error).
--
-- @since 0.1
permDeleteFromTrash ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  Bool ->
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m Bool
permDeleteFromTrash force trashHome pathName = addNamespace "permDeleteFromTrash" $ do
  pathDatas <- findPathData trashHome pathName

  anyFailedRef <- newIORef False
  let deleteFn pathData = do
        $(logDebug) ("Deleting: " <> T.pack (pathData ^. (#fileName % #unPathI)))
        if force
          then -- NOTE: Technically don't need the pathdata if force is on, since we have
          -- the path and can just delete it. Nevertheless, we retrieve the pathData
          -- so that force does not change the semantics i.e. can only delete
          -- "well-behaved" paths, and we don't have to do a redundant file/directory
          -- check.
            deleteFn' pathData
          else do
            -- NOTE:
            -- - No buffering on input so we can read a single char w/o requiring a
            --   newline to end the input (which then gets passed to getChar, which
            --   interferes with subsequent calls).
            --
            -- - No buffering on output so the "Permanently delete..." string gets
            --   printed w/o the newline.
            noBuffering

            let pdStr = (renderStrict . layoutCompact . (line <>) . pretty) pathData
            putTextLn pdStr
            putStr "Permanently delete (y/n)? "
            c <- Ch.toLower <$> getChar
            if
                | c == 'y' -> deleteFn' pathData *> putStrLn ""
                | c == 'n' -> putStrLn ""
                | otherwise -> putStrLn ("\nUnrecognized: " <> [c])

  -- Need our own error handling here since if we are deleting multiple
  -- wildcard matches we want success/failure to be independent.
  for_ pathDatas $ \pathData ->
    deleteFn pathData `catchAnyCS` \ex -> do
      writeIORef anyFailedRef True
      $(logWarn) (T.pack $ displayNoCS ex)
      putStrLn $
        mconcat
          [ "Error deleting path '",
            pathData ^. (#fileName % #unPathI),
            "': ",
            displayNoCS ex
          ]
  readIORef anyFailedRef
  where
    deleteFn' pd = do
      let delFn = PathData.getDeleteFn pd
          MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)
          MkPathI trashInfoPath' = Env.getTrashInfoPath trashHome (pd ^. #fileName)

      delFn trashPath'
      $(logDebug) ("Deleted: " <> showt pd)
      removeFile trashInfoPath'

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
-- Returns 'True' if any failed. In this case, the error has already been
-- reported, so this is purely for signaling (i.e. should we exit with
-- an error).
--
-- @since 0.1
restoreTrashToOriginal ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m Bool
restoreTrashToOriginal trashHome pathName = addNamespace "restoreTrashToOriginal" $ do
  -- 1. Get path info
  pathDatas <- findPathData trashHome pathName

  anyFailedRef <- newIORef False
  let restoreFn pd = do
        let originalPath = pd ^. #originalPath
            fileName = pd ^. #fileName

        $(logDebug) ("Restoring: " <> T.pack (fileName ^. #unPathI))

        -- 2. Verify original path is empty
        exists <- PathData.originalPathExists pd
        when exists $
          throwCS $
            MkRestoreCollisionE fileName originalPath

        restoreFn' pd

  -- Need our own error handling here since if we are restoring multiple
  -- wildcard matches we want success/failure to be independent.
  for_ pathDatas $ \pd ->
    restoreFn pd `catchAnyCS` \ex -> do
      writeIORef anyFailedRef True
      $(logWarn) (T.pack $ displayNoCS ex)
      putStrLn $
        mconcat
          [ "Error restoring path '",
            pd ^. (#fileName % #unPathI),
            "': ",
            displayNoCS ex
          ]
  readIORef anyFailedRef
  where
    restoreFn' pd = do
      let MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)
          MkPathI trashInfoPath' = Env.getTrashInfoPath trashHome (pd ^. #fileName)

      -- 3. Attempt restore
      mvPathData pd trashPath' (pd ^. #originalPath % #unPathI)

      -- 4. Delete info
      --
      -- NOTE: We do not do any error handling here as at this point we have
      -- accomplished our goal: restore the file. That the trash is now out of sync
      -- is bad, but there isn't anything we can do other than alert the user.
      removeFile trashInfoPath'

findOnePathData ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m PathData
findOnePathData trashHome pathName = do
  pathInfoExists <- doesFileExist trashInfoPath'
  unless pathInfoExists $
    throwCS $
      MkTrashEntryNotFoundE pathName trashInfoPath

  contents <- readBinaryFile trashInfoPath'
  pathData <- case PathData.decode pathName contents of
    Left err -> throwCS $ MkInfoDecodeE trashInfoPath contents err
    Right pd -> pure pd

  let existsFn = PathData.getExistsFn pathData
  pathExists <- existsFn trashPath'
  unless pathExists $
    throwCS $
      MkTrashEntryPathNotFoundE trashHome pathName

  pure pathData
  where
    MkPathI trashPath' = Env.getTrashPath trashHome pathName
    trashInfoPath@(MkPathI trashInfoPath') = Env.getTrashInfoPath trashHome pathName

findManyPathData ::
  ( HasCallStack,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
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

mvPathData ::
  ( HasCallStack,
    MonadPathWriter m
  ) =>
  PathData ->
  Path ->
  Path ->
  m ()
mvPathData pd = renameFn
  where
    renameFn = case pd ^. #pathType of
      PathTypeFile -> renameFile
      PathTypeDirectory -> renameDirectory

noBuffering :: (HasCallStack, MonadHandleWriter m) => m ()
noBuffering = buffOff IO.stdin *> buffOff IO.stdout
  where
    buffOff h = hSetBuffering h NoBuffering

-- | Searches for the given trash name in the trash.
--
-- @since 0.1
findPathData ::
  ( HasCallStack,
    MonadFileReader m,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryFileName ->
  m (NESeq PathData)
findPathData trashHome pathName@(MkPathI pathName') = addNamespace "findPathData" $ do
  if
      -- 1. Found a (n unescaped) wildcard; findMany (findMany handles the case
      -- where pathName also includes the sequence \\*).
      | hasWildcard pathName' -> findManyPathData trashHome pathName
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
      | otherwise -> NESeq.singleton <$> findOnePathData trashHome pathName
  where
    pathNameTxt = T.pack pathName'

    hasWildcard [] = False
    -- escaped; ignore
    hasWildcard ('\\' : '*' : xs) = hasWildcard xs
    hasWildcard ('*' : _) = True
    hasWildcard (_ : xs) = hasWildcard xs
