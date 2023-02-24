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
    decode,

    -- * Elimination
    encode,
    getRenameFn,
    getDeleteFn,

    -- * Existence
    trashPathExists,
    originalPathExists,
    getExistsFn,

    -- * Validation
    throwIfRoot,

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
    pathDataToTrashInfoPath,
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Asn
import Data.ByteString.Lazy qualified as BSL
import Data.Char qualified as Ch
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import Effects.FileSystem.PathWriter (removeFile)
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp, toText)
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( PathNotFoundE (MkPathNotFoundE),
    RenameDuplicateE (MkRenameDuplicateE),
    RootE (MkRootE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U
import System.FilePath qualified as FP

-- | Provides the serialized instance for PathData.
--
-- The goal is to serialize PathData, but we do not want to include fileName,
-- as that should be derived from the filename itself. Here are some ideas:
--
-- 1. Use a custom type (e.g. PathData') for the instance which leaves
-- out the fileName field.
--
-- Pro: Simple, does not change anything to the PathData type, entirely
--      internal.
-- Con: Duplicated fields.
--
-- 2. Same as 1, except have PathData directly depend on PathData'.
--
-- Pro: Simple, no duplicated fields.
-- Con: Changes PathData, code that depends on PathData details (e.g. tests)
--      will have to change. Will want manual optics instance so lens still
--      work (e.g. pathData ^. #size).
--
-- 3. Change PathData to Maybe and ignore it in (de)serialization.
--
-- Pro: Simple
-- Con: Code that relies on PathName being present now has to deal with
--      possible failures.
--
-- 4. Higher-kinded data i.e. PathData has field: fileName :: f Path. f will
--    be Maybe for serialization and reduce to Path for main code. Export
--    type PathData = PathDataF (Decoded).
--
-- Pro: Mostly not change PathData. No field duplication.
-- Con: Complex, details leak through (PathData now a type synonym to a more
--      complicated type).
--
-- We choose 1.
--
-- @since 0.1
data PathData' = MkPathData'
  { pathType :: !PathType,
    originalPath :: !(PathI OriginalPath),
    size :: !(Bytes B Natural),
    created :: !Timestamp
  }
  deriving stock (Generic, Show)

-- | @since 0.1
makeFieldLabelsNoPrefix ''PathData'

-- | @since 0.1
instance FromJSON PathData' where
  parseJSON = Asn.withObject "PathData'" $ \pd ->
    MkPathData'
      <$> pd .: "type"
      <*> pd .: "original"
      <*> (MkBytes <$> pd .: "size")
      <*> pd .: "created"

-- | @since 0.1
instance ToJSON PathData' where
  toJSON pd =
    Asn.object
      [ "type" .= (pd ^. #pathType),
        "original" .= (pd ^. #originalPath),
        "size" .= (pd ^. #size % _MkBytes),
        "created" .= (pd ^. #created)
      ]

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
  uniqPath <- mkUniqPath (Env.getTrashPath trashHome (Paths.reindex fileName))
  let uniqName = Paths.liftPathI FP.takeFileName uniqPath

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
          else throwCS $ MkPathNotFoundE (originalPath ^. #unPathI)

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
    MkPathData
      { fileName = fileName',
        originalPath = originalPath',
        pathType,
        size,
        created = currTime
      }

-- | Encodes the 'PathData' to a bytestring, ignoring the 'fieldName'
-- field.
--
-- @since 0.1
encode :: PathData -> ByteString
encode = BSL.toStrict . Asn.encode . fromPD
  where
    fromPD pd =
      MkPathData'
        { pathType = pd ^. #pathType,
          originalPath = pd ^. #originalPath,
          size = pd ^. #size,
          created = pd ^. #created
        }

-- | Decodes the bytestring to the 'PathData', using the passed trashName
-- as the 'fieldName'.
--
-- @since 0.1
decode :: PathI TrashName -> ByteString -> Either String PathData
decode name bs = case result of
  Right pd -> Right $ toPD pd
  Left err -> Left err
  where
    result = Asn.eitherDecode' (BSL.fromStrict bs)
    toPD :: PathData' -> PathData
    toPD pd' =
      MkPathData
        { fileName = name,
          pathType = pd' ^. #pathType,
          originalPath = pd' ^. #originalPath,
          size = pd' ^. #size,
          created = pd' ^. #created
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
  PathI TrashPath ->
  m (PathI TrashPath)
mkUniqPath fp = do
  b <- Paths.applyPathI doesPathExist fp
  if b
    then go 1
    else pure fp
  where
    go :: (HasCallStack) => Word16 -> m (PathI TrashPath)
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
sortName = mapOrd (fmap Ch.toLower . view (#fileName % #unPathI))

-- | Sorts by the name.
--
-- @since 0.1
sortSize :: PathData -> PathData -> Ordering
sortSize = mapOrd (view #size)

mapOrd :: (Ord b) => (a -> b) -> a -> a -> Ordering
mapOrd f x y = f x `compare` f y

-- | Returns 'True' if the 'PathData'\'s @fileName@ corresponds to a real path
-- that exists in 'TrashHome'.
--
-- @since 0.1
trashPathExists ::
  (MonadPathReader m, HasCallStack) =>
  PathI TrashHome ->
  PathData ->
  m Bool
trashPathExists trashHome pd = existsFn trashPath'
  where
    MkPathI trashPath' = Env.getTrashPath trashHome (pd ^. #fileName)
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
pathDataToTrashPath trashHome = Env.getTrashPath trashHome . view #fileName

-- | Gives the 'PathData'\'s full trash path in the given 'TrashHome'.
--
-- @since 0.1
pathDataToTrashInfoPath :: PathI TrashHome -> PathData -> PathI TrashInfoPath
pathDataToTrashInfoPath trashHome = Env.getTrashInfoPath trashHome . view #fileName

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

-- | @since 0.1
throwIfRoot :: (HasCallStack, MonadThrow m) => PathData -> m ()
throwIfRoot pd = when (isRoot pd) (throwCS MkRootE)

-- | @since 0.1
getRenameFn :: (MonadPathWriter m) => PathData -> Path -> Path -> m ()
getRenameFn pd = case pd ^. #pathType of
  PathTypeFile -> renameFile
  PathTypeDirectory -> renameDirectory

-- | @since 0.1
getDeleteFn :: (MonadPathWriter m) => PathData -> Path -> m ()
getDeleteFn pd = case pd ^. #pathType of
  PathTypeFile -> removeFile
  PathTypeDirectory -> removeDirectoryRecursive

-- | @since 0.1
getExistsFn :: (MonadPathReader m) => PathData -> Path -> m Bool
getExistsFn pd = case pd ^. #pathType of
  PathTypeFile -> doesFileExist
  PathTypeDirectory -> doesDirectoryExist

isRoot :: PathData -> Bool
isRoot pd =
  -- NOTE: Probably overly paranoid to check the file name too, but might as
  -- well...if root somehow ends up as the file name then something has likely
  -- gone wrong, so let's abort so the user can figure it out.
  Paths.isRoot (pd ^. #originalPath)
    || Paths.isRoot (pd ^. #fileName)
