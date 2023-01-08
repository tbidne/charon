{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides metadata functionality.
--
-- @since 0.1
module SafeRm.Data.Metadata
  ( Metadata (..),
    toMetadata,
  )
where

import Data.Bytes (SomeSize)
import Data.Bytes qualified as Bytes
import Data.HashMap.Strict qualified as Map
import Data.List qualified as L
import Data.Text qualified as T
import Effects.MonadLoggerNamespace (MonadLoggerNamespace, addNamespace)
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import Numeric.Literal.Rational (FromRational (afromRational))
import PathSize (PathSizeResult (..), pathSizeRecursive)
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashIndex, TrashLog),
  )
import SafeRm.Exception
  ( IndexSizeMismatchE (MkIndexSizeMismatchE),
    PathNotFoundE (MkPathNotFoundE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U
import System.FilePath qualified as FP

-- | Holds trash metadata.
--
-- @since 0.1
data Metadata = MkMetadata
  { -- | Number of top level entries in the trash index. This should be the
    -- same as the index length.
    --
    -- @since 0.1
    numEntries :: !Natural,
    -- | Number of total files in the trash.
    --
    -- @since 0.1
    numFiles :: !Natural,
    -- | Size of the log file.
    --
    -- @since 0.1
    logSize :: !(SomeSize Double),
    -- | Total size of the trash directory.
    --
    -- @since 0.1
    size :: !(SomeSize Double)
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
      NFData
    )

makeFieldLabelsNoPrefix ''Metadata

-- | @since 0.1
instance Semigroup Metadata where
  MkMetadata a b c d <> MkMetadata a' b' c' d' =
    MkMetadata (a + a') (b + b') (c .+. c') (d .+. d')

-- | @since 0.1
instance Monoid Metadata where
  mempty = MkMetadata 0 0 zero zero

-- | @since 0.1
instance Pretty Metadata where
  pretty stats = vsep strs <+> line
    where
      strs =
        [ "Entries:     " <+> pretty (stats ^. #numEntries),
          "Total Files: " <+> pretty (stats ^. #numFiles),
          "Log size:    " <+> pretty (U.formatBytes $ stats ^. #logSize),
          "Size:        " <+> pretty (U.formatBytes $ stats ^. #size)
        ]

-- | Returns metadata for the trash directory.
--
-- @since 0.1
toMetadata ::
  ( HasCallStack,
    MonadCallStack m,
    MonadFileReader m,
    MonadPathReader m,
    MonadPathSize m,
    MonadLoggerNamespace m,
    MonadTerminal m
  ) =>
  (PathI TrashHome, PathI TrashIndex, PathI TrashLog) ->
  m Metadata
toMetadata (trashHome@(MkPathI th), trashIndex, trashLog) =
  addNamespace "toMetadata" $ do
    -- Index size
    index <- view #unIndex <$> Index.readIndex trashIndex
    let numIndex = Map.size index
    $(logDebug) ("Index size: " <> showt numIndex)

    -- Num entries
    numEntries <- foldl' countFiles 0 <$> listDirectory th
    $(logDebug) ("Num entries: " <> showt numEntries)

    -- Log size
    let logPath = trashLog ^. #unPathI
    logExists <- doesFileExist logPath
    logSize <-
      if logExists
        then do
          fmap (Bytes.normalize . toDouble . MkBytes @B) $
            pathSizeRecursive logPath >>= \case
              PathSizeSuccess n -> pure n
              PathSizePartial errs n -> do
                -- We received a value but had some errors.
                putStrLn "Encountered errors retrieving size. See logs."
                for_ errs $ \e -> $(logError) (T.pack $ displayCallStack e)
                pure n
              PathSizeFailure errs -> do
                -- Received error, no value.
                putStrLn "Could not retrieve size, defaulting to 0. See logs."
                for_ errs $ \e -> $(logError) (T.pack $ displayCallStack e)
                pure 0
        else do
          $(logDebug) "Log does not exist"
          pure (afromRational 0)

    -- Summed size
    allFiles <- filter (not . skipFile) <$> getAllFiles th
    allSizes <- toDouble <$> foldl' sumFileSizes (pure zero) allFiles
    let numFiles = length allFiles
        size = Bytes.normalize allSizes

    $(logDebug) ("Num all files: " <> showt numFiles)
    $(logDebug) ("Total size: " <> showt size)

    -- NOTE: Verify that sizes are the same. Because reading the index verifies
    -- that there are no duplicate entries and each entry corresponds to a real
    -- trash path, this guarantees that the index exactly corresponds to the
    -- trash state.
    when (numEntries /= numIndex) $
      throwWithCallStack $
        MkIndexSizeMismatchE trashHome numFiles numIndex

    pure $
      MkMetadata
        { numEntries = toNat numEntries,
          numFiles = toNat numFiles,
          logSize,
          size
        }
  where
    sumFileSizes macc f = do
      !acc <- macc
      sz <- (MkBytes @B) <$> getFileSize f
      pure $ acc .+. sz
    toDouble :: Bytes s Natural -> Bytes s Double
    toDouble = fmap fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

    countFiles :: Int -> FilePath -> Int
    countFiles !acc fp
      | skipFile fp = acc
      | otherwise = acc + 1

    skipFile fp = FP.takeFileName fp `L.elem` [".log", ".index.csv"]

getAllFiles ::
  ( MonadPathReader m,
    HasCallStack,
    MonadCallStack m
  ) =>
  FilePath ->
  m [FilePath]
getAllFiles fp =
  doesFileExist fp >>= \case
    True -> pure [fp]
    False ->
      doesDirectoryExist fp >>= \case
        True ->
          listDirectory fp
            >>= fmap join
              . traverse (getAllFiles . (fp </>))
        False -> throwWithCallStack $ MkPathNotFoundE fp
