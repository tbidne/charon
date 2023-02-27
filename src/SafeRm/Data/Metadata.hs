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
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..), pathSizeRecursive)
import GHC.Real (Integral)
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import Numeric.Literal.Rational (FromRational (afromRational))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashLog),
  )
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( PathNotFoundE (MkPathNotFoundE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U

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
    MonadFileReader m,
    MonadPathReader m,
    MonadPathSize m,
    MonadLoggerNS m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  (PathI TrashHome, PathI TrashLog) ->
  m Metadata
toMetadata (trashHome, trashLog) =
  addNamespace "toMetadata" $ do
    -- Index size
    index <- view #unIndex <$> Index.readIndex trashHome
    let numIndex = length index
    $(logDebug) ("Index size: " <> showt numIndex)

    -- Num entries
    numEntries <- foldl' (\acc _ -> acc + 1) 0 <$> listDirectory trashPathsDir
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
                for_ errs $ \e -> $(logError) (T.pack $ displayException e)
                pure n
        else do
          $(logDebug) "Log does not exist"
          pure (afromRational 0)

    -- Summed size
    allFiles <- getAllFiles trashPathsDir
    allSizes <- toDouble <$> foldl' sumFileSizes (pure zero) allFiles
    let numFiles = length allFiles
        size = Bytes.normalize allSizes

    $(logDebug) ("Num all files: " <> showt numFiles)
    $(logDebug) ("Total size: " <> showt size)

    -- NOTE: If the index is successfully read then we have verified that
    -- all invariants are preserved i.e. bijection between /paths and /info.

    pure $
      MkMetadata
        { numEntries = toNat numEntries,
          numFiles = toNat numFiles,
          logSize,
          size
        }
  where
    MkPathI trashPathsDir = Env.getTrashPathDir trashHome

    sumFileSizes macc f = do
      !acc <- macc
      sz <- (MkBytes @B) <$> getFileSize f
      pure $ acc .+. sz
    toDouble :: (Integral a) => Bytes s a -> Bytes s Double
    toDouble = fmap fromIntegral
    toNat :: Int -> Natural
    toNat = fromIntegral

getAllFiles ::
  ( HasCallStack,
    MonadPathReader m,
    MonadThrow m
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
        False -> throwCS $ MkPathNotFoundE fp
