{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
--
-- @since 0.1
module SafeRm.Data.PathData
  ( PathData (..),

    -- * Creation
    toPathData,
    mvOriginalToTrash,

    -- * Existence
    trashPathExists,
    originalPathExists,

    -- * Deletion
    mvTrashToOriginal,
    deletePathData,

    -- * Sorting

    -- ** High level
    sortCreatedName,
    sortNameCreated,
    sortSizeName,

    -- ** Low level
    sortReverse,
    sortCreated,
    sortName,
    sortSize,

    -- * Formatting
    PathDataFormat (..),
    formatMultiLine,
    formatTabularHeader,
    formatTabularRow,
    reservedLineLen,

    -- * Miscellaneous
    headerNames,
    pathDataToTrashPath,
  )
where

import Data.Bytes (_MkBytes)
import Data.Csv
  ( DefaultOrdered (headerOrder),
    FromNamedRecord,
    FromRecord,
    ToNamedRecord,
    ToRecord,
    (.!),
    (.:),
    (.=),
  )
import Data.Csv qualified as Csv
import Data.HashMap.Strict qualified as Map
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (OriginalPath, TrashHome, TrashName, TrashPath),
    (<//>),
  )
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp, toText)
import SafeRm.Exception
  ( PathNotFoundE (MkPathNotFoundE),
    RenameDuplicateE (MkRenameDuplicateE),
    RestoreCollisionE (MkRestoreCollisionE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U
import System.FilePath qualified as FP

-- | Data for a path.
--
-- @since 0.1
data PathData = MkPathData
  { -- | The type of the path.
    --
    -- @since 0.1
    pathType :: !PathType,
    -- | The path to be used in the trash directory.
    --
    -- @since 0.1
    fileName :: !(PathI TrashName),
    -- | The original path on the file system.
    --
    -- @since 0.1
    originalPath :: !(PathI OriginalPath),
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
makeFieldLabelsNoPrefix ''PathData

-- | @since 0.1
instance FromRecord PathData where
  parseRecord m =
    MkPathData
      <$> m .! 0
      <*> m .! 1
      <*> m .! 2
      <*> (MkBytes <$> m .! 3)
      <*> m .! 4

-- | @since 0.1
instance FromNamedRecord PathData where
  parseNamedRecord m =
    MkPathData
      <$> m .: "type"
      <*> m .: "fileName"
      <*> m .: "original"
      <*> (MkBytes <$> m .: "size")
      <*> m .: "created"

-- | @since 0.1
instance ToRecord PathData where
  toRecord pd =
    [ Csv.toField (pd ^. #pathType),
      Csv.toField (pd ^. #fileName),
      Csv.toField (pd ^. #originalPath),
      Csv.toField (pd ^. #size % _MkBytes),
      Csv.toField (pd ^. #created)
    ]

-- | @since 0.1
instance ToNamedRecord PathData where
  toNamedRecord pd = Map.fromList $ zipWith (flip ($)) headerNames labelFn
    where
      labelFn =
        [ \x -> x .= (pd ^. #pathType),
          \x -> x .= (pd ^. #fileName),
          \x -> x .= (pd ^. #originalPath),
          \x -> x .= (pd ^. #size % _MkBytes),
          \x -> x .= (pd ^. #created)
        ]

-- | @since 0.1
instance DefaultOrdered PathData where
  headerOrder _ = headerNames

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
    MonadLogger m,
    MonadPathReader m,
    MonadPathSize m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Timestamp ->
  PathI TrashHome ->
  PathI OriginalPath ->
  m PathData
toPathData currTime trashHome origPath = do
  originalPath <- Paths.liftPathIF' canonicalizePath origPath
  -- NOTE: need to get the file name here because fp could refer to an
  -- absolute path. In this case, </> returns the 2nd arg which is absolutely
  -- not what we want.
  --
  -- This works for directories too because canonicalizePath drops the
  -- trailing slashes.
  let fileName = Paths.liftPathI' FP.takeFileName originalPath
  uniqPath <- mkUniqPath (trashHome <//> fileName)
  let uniqName = Paths.liftPathI' FP.takeFileName uniqPath

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
          else throwWithCallStack $ MkPathNotFoundE (originalPath ^. #unPathI)

  size <-
    fmap (MkBytes @B) $
      pathSizeRecursive (originalPath ^. #unPathI) >>= \case
        PathSizeSuccess n -> pure n
        PathSizePartial errs n -> do
          -- We received a value but had some errors.
          putStrLn "Encountered errors retrieving size. See logs."
          for_ errs $ \e -> $(logError) (T.pack $ displayCallStack e)
          pure n

  pure $
    MkPathData
      { fileName = fileName',
        originalPath = originalPath',
        pathType,
        size,
        created = currTime
      }

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
  PathI TrashName ->
  m (PathI TrashName)
mkUniqPath fp = do
  b <- Paths.applyPathI doesPathExist fp
  if b
    then go 1
    else pure fp
  where
    go :: HasCallStack => Word16 -> m (PathI TrashName)
    go !counter
      | counter == maxBound =
          throwWithCallStack $ MkRenameDuplicateE fp
      | otherwise = do
          let fp' = fp <> MkPathI (mkSuffix counter)
          b <- Paths.applyPathI doesPathExist fp'
          if b
            then go (counter + 1)
            else pure fp'
    mkSuffix i = " (" <> show i <> ")"

-- | Sorts by the created date then the name.
--
-- @since 0.1
sortCreatedName :: PathData -> PathData -> Ordering
sortCreatedName x y = case sortCreated x y of
  EQ -> sortName x y
  other -> other

sortNameCreated :: PathData -> PathData -> Ordering
sortNameCreated x y = case sortName x y of
  EQ -> sortCreated x y
  other -> other

sortSizeName :: PathData -> PathData -> Ordering
sortSizeName x y = case sortSize x y of
  EQ -> sortName x y
  other -> other

sortReverse :: (a -> b -> Ordering) -> a -> b -> Ordering
sortReverse f x y = case f x y of
  EQ -> EQ
  LT -> GT
  GT -> LT

-- | Sorts by the created date.
--
-- @since 0.1
sortCreated :: PathData -> PathData -> Ordering
sortCreated = mapOrd (view #created)

-- | Sorts by the name.
--
-- @since 0.1
sortName :: PathData -> PathData -> Ordering
sortName = mapOrd (view #fileName)

-- | Sorts by the name.
--
-- @since 0.1
sortSize :: PathData -> PathData -> Ordering
sortSize = mapOrd (view #size)

mapOrd :: Ord b => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y

-- | Moves the 'PathData'\'s @fileName@ back to its @originalPath@.
--
-- @since 0.1
mvTrashToOriginal ::
  ( HasCallStack,
    MonadPathReader m,
    MonadPathWriter m,
    MonadThrow m
  ) =>
  PathI TrashHome ->
  PathData ->
  m ()
mvTrashToOriginal (MkPathI trashHome) pd = do
  exists <- originalPathExists pd
  when exists $
    throwWithCallStack $
      MkRestoreCollisionE fileName originalPath
  renameFn trashPath (pd ^. #originalPath % #unPathI)
  where
    originalPath = pd ^. #originalPath
    fileName = pd ^. #fileName
    trashPath = trashHome </> (fileName ^. #unPathI)
    renameFn = case pd ^. #pathType of
      PathTypeFile -> renameFile
      PathTypeDirectory -> renameDirectory

-- | Permanently deletes the 'PathData'.
--
-- @since 0.1
deletePathData ::
  MonadPathWriter m =>
  PathI TrashHome ->
  PathData ->
  m ()
deletePathData (MkPathI trashHome) pd = removePathForcibly trashPath
  where
    trashPath = trashHome </> (pd ^. #fileName % #unPathI)

-- | Moves the 'PathData'\'s @originalPath@ to the trash.
--
-- @since 0.1
mvOriginalToTrash ::
  (MonadPathWriter m, HasCallStack) =>
  PathI TrashHome ->
  PathData ->
  m ()
mvOriginalToTrash trashHome pd =
  renameFn (pd ^. #originalPath % #unPathI) trashPath
  where
    MkPathI trashPath = pathDataToTrashPath trashHome pd
    renameFn = case pd ^. #pathType of
      PathTypeFile -> renameFile
      PathTypeDirectory -> renameDirectory

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists ::
  (MonadPathReader m, HasCallStack) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists (MkPathI trashHome) pd = existsFn trashPath
  where
    trashPath = trashHome </> (pd ^. #fileName % #unPathI)
    existsFn = case pd ^. #pathType of
      PathTypeFile -> doesFileExist
      PathTypeDirectory -> doesDirectoryExist

-- | Returns 'True' if the 'PathData'\'s @originalPath@ corresponds to a real
-- path that exists.
--
-- @since 0.1
originalPathExists ::
  (MonadPathReader m, HasCallStack) =>
  PathData ->
  m Bool
originalPathExists pd = existsFn (pd ^. #originalPath % #unPathI)
  where
    existsFn = case pd ^. #pathType of
      PathTypeFile -> doesFileExist
      PathTypeDirectory -> doesDirectoryExist

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashPath :: PathI TrashHome -> PathData -> PathI TrashPath
pathDataToTrashPath trashHome = (trashHome <//>) . view #fileName

-- | Determines how to format a textual 'PathData'.
--
-- @since 0.1
data PathDataFormat
  = -- | Formats each file on its own line.
    --
    -- @since 0.1
    FormatMultiline
  | -- | Formats all fields on the same line.
    --
    -- @since 0.1
    FormatTabular Natural Natural
  | -- | Like 'FormatTabular', except it attempts to detect the best
    -- column widths automatically.
    --
    -- @since 0.1
    FormatTabularAuto
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
formatMultiLine :: PathData -> Text
formatMultiLine pd = T.intercalate "\n" strs
  where
    strs = zipWith (flip ($)) headerNames labelFn
    labelFn =
      [ \x -> x <> ":     " <> typeToText (pd ^. #pathType),
        \x -> x <> ":     " <> T.pack (pd ^. #fileName % #unPathI),
        \x -> x <> ": " <> T.pack (pd ^. #originalPath % #unPathI),
        \x -> x <> ":     " <> U.normalizedFormat (pd ^. #size),
        \x -> x <> ":  " <> toText (pd ^. #created)
      ]

typeToText :: PathType -> Text
typeToText PathTypeFile = "File"
typeToText PathTypeDirectory = "Directory"

-- | @since 0.1
formatTabularHeader :: Natural -> Natural -> Text
formatTabularHeader nameLen origLen =
  mconcat
    [ fixLen nameLen "Name",
      sep,
      fixLen 10 "Type",
      sep,
      fixLen 7 "Size",
      sep,
      fixLen origLen "Original",
      sep,
      -- No need to fix the length here as Created is the last column
      "Created",
      "\n",
      titleLen
    ]
  where
    -- extra 12 is from the separators
    totalLen = nameLen + origLen + reservedLineLen
    titleLen = T.replicate (fromIntegral totalLen) "-"

-- | For tabular formatting, this is the necessary width for the fixed
-- columns:
--
-- type: 10
-- size: 7
-- created: 19
-- separators: 12
--
-- NOTE: The separators includes the adjacent whitespace i.e. one separator
-- ' | ' counts for 3, and since we have 4 that makes 12.
--
-- This does not include the minimum necessary total space (i.e. minimum
-- 4 for name and 8 for original).
--
-- @since 0.1
reservedLineLen :: Natural
reservedLineLen = 10 + 7 + 19 + 12

-- | @since 0.1
formatTabularRow :: Natural -> Natural -> PathData -> Text
formatTabularRow nameLen origLen pd =
  mconcat
    [ fixLen' nameLen (pd ^. #fileName % #unPathI),
      sep,
      paddedType (pd ^. #pathType),
      sep,
      fixLen 7 (U.normalizedFormat $ pd ^. #size),
      sep,
      fixLen' origLen (pd ^. #originalPath % #unPathI),
      sep,
      toText (pd ^. #created)
    ]
  where
    paddedType PathTypeFile = "File      "
    paddedType PathTypeDirectory = "Directory "

sep :: Text
sep = " | "

fixLen' :: Natural -> String -> Text
fixLen' w s = fixLen w (T.pack s)

-- | @since 0.1
fixLen :: Natural -> Text -> Text
fixLen w t
  | w' < T.length t = T.take (w' - 3) t <> "..."
  | otherwise = t <> T.replicate (w' - T.length t) " "
  where
    w' = fromIntegral w
