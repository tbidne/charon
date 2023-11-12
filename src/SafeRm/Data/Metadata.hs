{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides metadata functionality.
module SafeRm.Data.Metadata
  ( Metadata (..),
    toMetadata,
    empty,
  )
where

import Data.Bytes (SomeSize)
import Data.Bytes qualified as Bytes
import GHC.Real (Integral)
import Numeric.Algebra (AMonoid (zero), ASemigroup ((.+.)))
import Numeric.Literal.Rational (FromRational (afromRational))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex (TrashHome, TrashLog),
  )
import SafeRm.Env (HasBackend)
import SafeRm.Env qualified as Env
import SafeRm.Exception
  ( FileNotFoundE (MkFileNotFoundE),
  )
import SafeRm.Prelude
import SafeRm.Utils qualified as U

-- | Holds trash metadata.
data Metadata = MkMetadata
  { -- | Number of top level entries in the trash index. This should be the
    -- same as the index length.
    numEntries :: Natural,
    -- | Number of total files in the trash.
    numFiles :: Natural,
    -- | Size of the log file.
    logSize :: SomeSize Double,
    -- | Total size of the trash directory.
    size :: SomeSize Double
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''Metadata

-- | Empty metadata.
empty :: Metadata
empty = MkMetadata 0 0 zero zero

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
toMetadata ::
  ( HasBackend env,
    HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadLoggerNS m,
    MonadReader env m,
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
        then Bytes.normalize . toDouble . MkBytes @B <$> getFileSize logPath
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
    -- all invariants are preserved i.e. bijection between /files and /info.

    pure
      $ MkMetadata
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
  OsPath ->
  m [OsPath]
getAllFiles fp =
  doesFileExist fp >>= \case
    True -> pure [fp]
    False ->
      doesDirectoryExist fp >>= \case
        True ->
          listDirectory fp
            >>= fmap join
            . traverse (getAllFiles . (fp </>))
        False -> throwCS $ MkFileNotFoundE fp
