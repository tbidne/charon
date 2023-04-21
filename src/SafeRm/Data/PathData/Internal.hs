{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.PathData.Internal
  ( PathData (..),
    toPathData,
    decode,
    encode,
    headerNames,
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Serialize (Serialize (..), decodeUnit)
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( EmptyPathE (MkEmptyPathE),
    FileNotFoundE (MkFileNotFoundE),
    RenameDuplicateE (MkRenameDuplicateE),
    RootE (MkRootE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U
import System.FilePath qualified as FP
import Text.Read qualified as TR

-- | Data for a path. Maintains an invariant that the original path is not
-- the root nor is it empty.
--
-- @since 0.1
data PathData = UnsafePathData
  { -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    fileName :: !(PathI TrashEntryFileName),
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !(PathI TrashEntryOriginalPath),
    -- | The size of the file or directory.
    --
    -- @since 0.1
    size :: !(Bytes B Natural),
    -- | Time this entry was created.
    --
    -- @since 0.1
    created :: !Timestamp
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      Hashable,
      -- | @since 0.1
      NFData
    )

-- | @since 0.1
makeFieldLabelsWith
  (noPrefixFieldLabels & generateUpdateableOptics .~ False)
  ''PathData

-- | @since 0.1
instance Pretty PathData where
  pretty pd = vsep strs <+> line
    where
      strs = zipWith (flip ($)) headerNames labelFn
      labelFn =
        [ \x -> x <> ":     " <+> pretty (pd ^. #pathType),
          \x -> x <> ":     " <+> pretty (pd ^. #fileName % #unPathI),
          \x -> x <> ": " <+> pretty (pd ^. #originalPath % #unPathI),
          \x -> x <> ":     " <+> pretty (U.normalizedFormat $ pd ^. #size),
          \x -> x <> ":  " <+> pretty (pd ^. #created)
        ]

-- | Header names.
--
-- @since 0.1
headerNames :: (IsList a, IsString (Exts.Item a)) => a
headerNames = ["Type", "Name", "Original", "Size", "Created"]

-- | For a given filepath, attempts to capture the following data:
--
-- * Canonical path.
-- * Unique name to be used in the trash directory.
-- * File/directory type.
--
-- @since 0.1
toPathData ::
  ( HasCallStack,
    MonadLoggerNS m,
    MonadPathReader m,
    MonadPathSize m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI TrashEntryOriginalPath ->
  m PathData
toPathData currTime trashHome origPath = addNamespace "toPathData" $ do
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
  (fileName', originalPath', pathType) <-
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
          else throwCS $ MkFileNotFoundE (originalPath ^. #unPathI)

  size <-
    fmap (MkBytes @B) $
      pathSizeRecursive (originalPath ^. #unPathI) >>= \case
        PathSizeSuccess n -> pure n
        PathSizePartial errs n -> do
          -- We received a value but had some errors.
          putStrLn "Encountered errors retrieving size. See logs."
          for_ errs $ \e -> $(logError) (T.pack $ displayException e)
          pure n

  pure $
    UnsafePathData
      { fileName = fileName',
        originalPath = originalPath',
        pathType,
        size,
        created = currTime
      }

instance Serialize PathData where
  type DecodeExtra PathData = PathI TrashEntryFileName

  encode :: PathData -> ByteString
  encode pd =
    C8.unlines
      [ "[Trash Info]",
        "Path=" <> encode (pd ^. (#originalPath)),
        "DeletionDate=" <> encode (pd ^. #created),
        "Size=" <> pd ^. (#size % _MkBytes % shown % packed % packedbs),
        "Type=" <> encode (pd ^. #pathType)
      ]

  decode :: PathI TrashEntryFileName -> ByteString -> Either String PathData
  decode name bs = do
    case C8.lines bs of
      [] -> Left "Received empty pathdata"
      (h : rest) | isHeader h -> do
        let mp = Map.fromList (fmap breakEq rest)

        originalPath <- decodeUnit =<< lookup "Path" mp
        created <- decodeUnit =<< lookup "DeletionDate" mp
        size <- decodeSize =<< lookup "Size" mp
        pathType <- decodeUnit =<< lookup "Type" mp

        Right $
          UnsafePathData
            { fileName = name,
              originalPath,
              pathType,
              size,
              created
            }
      _ -> Left $ "Did not receive header [Trash Info]: " <> bsToStr bs
    where
      isHeader = (== "[Trash Info]")

      decodeSize bs' = do
        let bytesStr = C8.unpack bs'
        case TR.readMaybe bytesStr of
          Nothing -> Left $ "Could not read bytes: " <> bytesStr
          Just n -> Right $ MkBytes n

      lookup k mp = case Map.lookup k mp of
        Nothing -> Left $ "Could not find key: " <> bsToStr k
        Just v -> Right v

-- | Ensures the filepath @p@ is unique. If @p@ collides with another path,
-- we iteratively try appending numbers, stopping once we find a unique path.
-- For example, for duplicate "file", we will try
--
-- @
-- "file1", "file2", "file3", ...
-- @
--
-- @since 0.1
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

-- | @since 0.1
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

breakEq :: ByteString -> (ByteString, ByteString)
breakEq bs = (left, right')
  where
    (left, right) = C8.break (== '=') bs
    right' = case C8.uncons right of
      Nothing -> ""
      Just (_, rest) -> rest
