{-# LANGUAGE TemplateHaskell #-}

-- | Interface for the trash directory
--
-- @since 0.1
module SafeRm.Trash
  ( -- * Trash directory
    createTrash,
    doesTrashExist,

    -- * Trash files
    mvOriginalToTrash,
    mvTrashToOriginal,
    deleteTrashPath,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Char qualified as Ch
import Data.Text qualified as T
import Effects.Exception (onException)
import Effects.FileSystem.HandleWriter (MonadHandleWriter (..))
import Effects.FileSystem.PathWriter (removeFile)
import Effects.System.Terminal (MonadTerminal (..))
import SafeRm.Data.PathData (PathData (..))
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env (HasTrashHome (..))
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( AesonDecodeE (..),
    RestoreCollisionE (MkRestoreCollisionE),
    TrashEntryNotFoundE (MkTrashEntryNotFoundE),
    TrashInfoDirNotFoundE (..),
    TrashPathDirNotFoundE (..),
    TrashPathNotFoundE (MkTrashPathNotFoundE),
  )
import SafeRm.Prelude
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
-- does not, throws 'TrashPathDirNotFoundE' or 'TrashInfoDirNotFoundE'.
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
      pathsExists <- doesDirectoryExist trashPathDir'
      unless pathsExists $
        throwCS $
          MkTrashPathDirNotFoundE trashHome

      infoExists <- doesDirectoryExist trashInfoDir'
      unless infoExists $
        throwCS $
          MkTrashInfoDirNotFoundE trashHome

      pure True

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
--
-- @since 0.1
mvOriginalToTrash ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileWriter m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathSize m,
    MonadPathWriter m,
    MonadTerminal m
  ) =>
  PathI TrashHome ->
  Timestamp ->
  PathI OriginalPath ->
  m ()
mvOriginalToTrash trashHome currTime path = addNamespace "mvOriginalToTrash" $ do
  -- 1. Transform path to PathData
  pd <- PathData.toPathData currTime trashHome path
  $(logDebug) ("Deleting: " <> showt pd)

  let MkPathI trashPath = PathData.pathDataToTrashPath trashHome pd
      MkPathI trashInfoPath = PathData.pathDataToTrashInfoPath trashHome pd

  -- 2. Check that it's not root
  PathData.throwIfRoot pd

  -- 3. Write info file
  --
  -- Perform this before the actual move to be safe i.e. path is only moved
  -- if info is already created.
  let json = PathData.encode pd
  writeBinaryFile trashInfoPath (BSL.toStrict json)

  -- 4. Move file to trash
  let moveFn = mvPathData pd (pd ^. #originalPath % #unPathI) trashPath

  -- 5. If move failed, roll back info file
  moveFn `onException` removeFile trashInfoPath
  $(logDebug) ("Moved to trash: " <> showt pd)

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
--
-- @since 0.1
deleteTrashPath ::
  ( HasCallStack,
    MonadFileReader m,
    MonadHandleWriter m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Bool ->
  PathI TrashHome ->
  PathI TrashName ->
  m ()
deleteTrashPath force trashHome pathName = addNamespace "deleteTrashPath" $ do
  $(logDebug) ("Deleting: " <> T.pack (pathName ^. #unPathI))

  pathData <- findPathData trashHome pathName
  if force
    then -- NOTE: Technically don't need the pathdata if force is on, since we have
    -- the path and can just delete it. Nevertheless, we retrieve the pathData
    -- so that force does not change the semantics i.e. can only delete
    -- "well-behaved" paths, and we don't have to do a redundant file/directory
    -- check.
      deleteFn pathData
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
          | c == 'y' -> deleteFn pathData *> putStrLn ""
          | c == 'n' -> putStrLn ""
          | otherwise -> putStrLn ("\nUnrecognized: " <> [c])
  where
    MkPathI trashPath' = Env.getTrashPath trashHome pathName
    MkPathI trashInfoPath' = Env.getTrashInfoPath trashHome pathName

    deleteFn pd = do
      let delFn = PathData.getDeleteFn pd
      delFn trashPath'
      $(logDebug) ("Deleted: " <> showt pd)
      removeFile trashInfoPath'

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
--
-- @since 0.1
mvTrashToOriginal ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadLoggerNamespace m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathI TrashHome ->
  PathI TrashName ->
  m ()
mvTrashToOriginal trashHome pathName = addNamespace "mvTrashToOriginal" $ do
  $(logDebug) ("Restoring: " <> T.pack (pathName ^. #unPathI))

  -- 1. Get path info
  pd <- findPathData trashHome pathName

  let originalPath = pd ^. #originalPath
      fileName = pd ^. #fileName

  -- 2. Verify original path is empty
  exists <- PathData.originalPathExists pd
  when exists $
    throwCS $
      MkRestoreCollisionE fileName originalPath

  -- 3. Attempt restore
  mvPathData pd trashPath' (pd ^. #originalPath % #unPathI)
  $(logDebug) ("Restored: " <> showt pd)

  -- 4. Delete info
  --
  -- NOTE: We do not do any error handling here as at this point we have
  -- accomplished our goal: restore the file. That the trash is now out of sync
  -- is bad, but there isn't anything we can do other than alert the user.
  removeFile trashInfoPath'
  where
    MkPathI trashPath' = Env.getTrashPath trashHome pathName
    MkPathI trashInfoPath' = Env.getTrashInfoPath trashHome pathName

findPathData ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashName ->
  m PathData
findPathData trashHome pathName = do
  pathInfoExists <- doesFileExist trashInfoPath'
  unless pathInfoExists $
    throwCS $
      MkTrashEntryNotFoundE pathName trashInfoPath

  contents <- readBinaryFile trashInfoPath'
  pathData <- case PathData.decode pathName (BSL.fromStrict contents) of
    Left err -> throwCS $ MkAesonDecodeE contents err
    Right pd -> pure pd

  let existsFn = PathData.getExistsFn pathData
  pathExists <- existsFn trashPath'
  unless pathExists $
    throwCS $
      MkTrashPathNotFoundE trashHome pathName

  pure pathData
  where
    MkPathI trashPath' = Env.getTrashPath trashHome pathName
    trashInfoPath@(MkPathI trashInfoPath') = Env.getTrashInfoPath trashHome pathName

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
