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
  )
where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Asn
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Exts (IsList)
import GHC.Exts qualified as Exts
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths (PathI (MkPathI), PathIndex (..))
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.Timestamp (Timestamp)
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( EmptyPathE (MkEmptyPathE),
    PathNotFoundE (MkPathNotFoundE),
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
  PathI OriginalPath ->
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
    UnsafePathData
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
      UnsafePathData
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
