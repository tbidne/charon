{-# LANGUAGE TemplateHaskell #-}

module SafeRm.Data.PathData.Common
  ( getPathInfo,
    lookup,
  )
where

import Data.HashMap.Strict qualified as Map
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( EmptyPathE (MkEmptyPathE),
    FileNotFoundE (MkFileNotFoundE),
    PathNotFileDirE (..),
    RenameDuplicateE (MkRenameDuplicateE),
    RootE (MkRootE),
  )
import SafeRm.Prelude
import System.FilePath qualified as FP

-- | For a given path, retrieves its unique trash entry file name,
-- original path, and type.
getPathInfo ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m (PathI TrashEntryFileName, PathI TrashEntryOriginalPath, PathType)
getPathInfo trashHome origPath = do
  $(logDebug) $ "Retreiving path data: '" <> Paths.toText origPath <> "'"

  -- NOTE: It is VERY important that this check is first i.e. we perform it
  -- on the original given path, before any processing. As an example of
  -- what can go wrong, if someone attempts to delete a blank path
  -- (i.e. sr d ""), then canonicalizePath will turn this into the current
  -- directory, as in, will delete the entire working directory. This is
  -- not what we want!
  throwIfIllegal origPath

  originalPath <- Paths.liftPathIF' canonicalizePath origPath

  $(logDebug) $ "Canonicalized: '" <> Paths.toText originalPath <> "'"

  -- NOTE: need to get the file name here because fp could refer to an
  -- absolute path. In this case, </> returns the 2nd arg which is absolutely
  -- not what we want.
  --
  -- This works for directories too because canonicalizePath drops the
  -- trailing slashes.
  let fileName = Paths.liftPathI' FP.takeFileName originalPath
  uniqPath <- mkUniqPath (Env.getTrashPath trashHome (Paths.reindex fileName))
  let uniqName = Paths.liftPathI FP.takeFileName uniqPath

  $(logDebug) $ "Unique name: '" <> Paths.toText uniqName <> "'"

  isFile <- Paths.applyPathI doesFileExist originalPath
  if isFile
    then pure (uniqName, originalPath, PathTypeFile)
    else do
      isDir <- Paths.applyPathI doesDirectoryExist originalPath
      if isDir
        then
          pure
            ( -- NOTE: ensure paths do not have trailing slashes so that we can
              -- ensure later lookups succeed (requires string equality)
              Paths.liftPathI' FP.dropTrailingPathSeparator uniqName,
              Paths.liftPathI' FP.dropTrailingPathSeparator originalPath,
              PathTypeDirectory
            )
        else do
          pathExists <- Paths.applyPathI doesPathExist originalPath
          if pathExists
            then throwCS $ MkPathNotFileDirE (originalPath ^. #unPathI)
            else throwCS $ MkFileNotFoundE (originalPath ^. #unPathI)

-- | Ensures the filepath @p@ is unique. If @p@ collides with another path,
-- we iteratively try appending numbers, stopping once we find a unique path.
-- For example, for duplicate "file", we will try
--
-- @
-- "file1", "file2", "file3", ...
-- @
mkUniqPath ::
  forall m.
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashEntryPath ->
  m (PathI TrashEntryPath)
mkUniqPath fp = do
  b <- Paths.applyPathI doesPathExist fp
  if b
    then go 1
    else pure fp
  where
    go :: (HasCallStack) => Word16 -> m (PathI TrashEntryPath)
    go !counter
      | counter == maxBound =
          throwCS $ MkRenameDuplicateE fp
      | otherwise = do
          let fp' = fp <> MkPathI (mkSuffix counter)
          b <- Paths.applyPathI doesPathExist fp'
          if b
            then go (counter + 1)
            else pure fp'
    mkSuffix i = " (" <> show i <> ")"

throwIfIllegal ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadThrow m
  ) =>
  PathI i ->
  m ()
throwIfIllegal p =
  addNamespace "throwIfIllegal" $
    if
        | Paths.isRoot p -> $(logError) "Path is root!" *> throwCS MkRootE
        | Paths.isEmpty p -> $(logError) "Path is empty!" *> throwCS MkEmptyPathE
        | otherwise -> pure ()

lookup :: ByteString -> HashMap ByteString b -> Either String b
lookup k mp = case Map.lookup k mp of
  Nothing -> Left $ "Could not find key: " <> bsToStr k
  Just v -> Right v
