{-# LANGUAGE TemplateHaskell #-}

module SafeRm.Backend.Default.Utils
  ( -- * Trash paths
    getTrashPathDir,
    getTrashInfoDir,
    getTrashPath,

    -- * Misc
    getPathInfo,
    parseTrashInfoMap,
    pathDataToType,
    lookup,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Effects.FileSystem.Utils qualified as FsUtils
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashDirFiles,
        TrashDirInfo,
        TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashEntryPath,
        TrashHome
      ),
    (<//>),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Exception
  ( EmptyPathE (MkEmptyPathE),
    FileNotFoundE (MkFileNotFoundE),
    PathNotFileDirE (MkPathNotFileDirE),
    RenameDuplicateE (MkRenameDuplicateE),
    RootE (MkRootE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U
import System.OsPath qualified as FP

-- | Retrieves the trash path dir.
getTrashPathDir :: PathI TrashHome -> PathI TrashDirFiles
getTrashPathDir trashHome = trashHome <//> MkPathI pathFiles

-- | Retrieves the trash info dir.
getTrashInfoDir :: PathI TrashHome -> PathI TrashDirInfo
getTrashInfoDir trashHome = trashHome <//> MkPathI pathInfo

getTrashPath :: PathI TrashHome -> PathI TrashEntryFileName -> PathI TrashEntryPath
getTrashPath trashHome name = trashHome <//> MkPathI pathFiles <//> name

-- | For a given path, retrieves its unique trash entry file name,
-- original path, and type.
--
-- NOTE: This function is __almost__ backend agnostic. AFAICT the only
-- part that uses internal knowledge is the call to getTrashPath.
-- If we ever write a non-default backend then we should extract that logic
-- to a function and move this fn to Utils, as the rest of the logic is
-- something we will want to use everywhere (e.g. throwIfIllegal).
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
  $(logDebug) $ "Retrieving path data: '" <> Paths.toText origPath <> "'"

  -- NOTE: It is VERY important that this check is first i.e. we perform it
  -- on the original given path, before any processing. As an example of
  -- what can go wrong, if someone attempts to delete a blank path
  -- (i.e. safe-rm d ""), then canonicalizePath will turn this into the current
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
  uniqPath <- mkUniqPath (getTrashPath trashHome (Paths.reindex fileName))
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
          counterStr <- mkSuffix counter
          let fp' = Paths.liftPathI' (<> counterStr) fp
          b <- Paths.applyPathI doesPathExist fp'
          if b
            then go (counter + 1)
            else pure fp'
    mkSuffix i = FsUtils.encodeFpToOsThrowM $ " (" <> show i <> ")"

throwIfIllegal ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadThrow m
  ) =>
  PathI i ->
  m ()
throwIfIllegal p = addNamespace "throwIfIllegal" $ do
  U.whenM (Paths.isRoot p) $ $(logError) "Path is root!" *> throwCS MkRootE

  U.whenM (Paths.isEmpty p) $ $(logError) "Path is empty!" *> throwCS MkEmptyPathE

-- | Parses a ByteString like:
--
-- @
-- [Trash Info]
-- k1=v1
-- k2=v2
-- ...
-- @
--
-- into a map of @{ ki => vi }@.
--
-- Verifies that the parameter key set is exactly the key set in the map.
parseTrashInfoMap ::
  -- | Expected keys
  HashSet ByteString ->
  -- | ByteString
  ByteString ->
  Either String (HashMap ByteString ByteString)
parseTrashInfoMap expectedKeys bs =
  case C8.lines bs of
    [] -> Left "Received empty pathdata"
    (h : rest) | isHeader h -> do
      let mp = Map.fromList (fmap U.breakEqBS rest)
          keys = Map.keysSet mp
          missingKeys = Set.difference expectedKeys keys
          missingKeysStr = C8.intercalate ", " $ Set.toList missingKeys
          unexpectedKeys = Set.difference keys expectedKeys
          unexpectedKeysStr = C8.intercalate ", " $ Set.toList unexpectedKeys

      unless (Set.null unexpectedKeys)
        $ Left
        $ "Unexpected keys: '"
        <> bsToStrLenient unexpectedKeysStr
        <> "'"

      unless (Set.null missingKeys)
        $ Left
        $ "Missing keys: '"
        <> bsToStrLenient missingKeysStr
        <> "'"

      Right mp
    _ -> Left $ "Did not receive header [Trash Info]: " <> bsToStr bs
  where
    isHeader = (== "[Trash Info]")

lookup :: ByteString -> HashMap ByteString b -> Either String b
lookup k mp = case Map.lookup k mp of
  Nothing -> Left $ "Could not find key: " <> bsToStr k
  Just v -> Right v

-- | Derives the 'PathType' from the 'PathData'.
--
-- __IMPORTANT:__ This function is only guaranteed to work if the 'PathData'
-- corresponds to an extant trash entry. In particular, if the 'PathData' has
-- not been created yet, this can fail.
pathDataToType ::
  ( Is k A_Getter,
    LabelOptic' "fileName" k a (PathI TrashEntryFileName),
    HasCallStack,
    MonadPathReader m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  a ->
  m PathType
pathDataToType trashHome pd = do
  fileExists <- doesFileExist path
  if fileExists
    then pure PathTypeFile
    else do
      dirExists <- doesDirectoryExist path
      if dirExists
        then pure PathTypeDirectory
        else do
          -- for a better error message
          pathExists <- doesPathExist path
          if pathExists
            then throwCS $ MkPathNotFileDirE path
            else throwCS $ MkFileNotFoundE path
  where
    MkPathI path = getTrashPath trashHome (pd ^. #fileName)
