{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides types.
module Charon.Data.PathType
  ( PathTypeW (..),
    deleteFn,
    existsFn,
    renameFn,
    copyPath,
  )
where

-- import Charon.Class.Serial (Serial (DecodeExtra, decode, encode))
import Charon.Prelude
import Codec.Serialise (Serialise (encode))
import Codec.Serialise qualified as Serialise
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Data.Text qualified as T
import Effects.FileSystem.PathWriter
  ( CopyDirConfig (MkCopyDirConfig),
    MonadPathWriter (removeFile),
    Overwrite (OverwriteDirectories),
    TargetName (TargetNameSrc),
  )
import Effects.FileSystem.PathWriter qualified as PW

-- | Wrapper for PathType, so that we can give a Hashable instance.
newtype PathTypeW = MkPathTypeW {unPathTypeW :: PathType}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

makeFieldLabelsNoPrefix ''PathTypeW

instance Hashable PathTypeW where
  hashWithSalt int (MkPathTypeW PathTypeFile) = hashWithSalt @Int int 1
  hashWithSalt int (MkPathTypeW PathTypeDirectory) = hashWithSalt @Int int 2
  hashWithSalt int (MkPathTypeW PathTypeSymbolicLink) = hashWithSalt @Int int 3

instance Pretty PathTypeW where
  pretty (MkPathTypeW PathTypeFile) = "File"
  pretty (MkPathTypeW PathTypeDirectory) = "Directory"
  pretty (MkPathTypeW PathTypeSymbolicLink) = "Symlink"

instance ToJSON PathTypeW where
  toJSON (MkPathTypeW PathTypeFile) = "f"
  toJSON (MkPathTypeW PathTypeDirectory) = "d"
  toJSON (MkPathTypeW PathTypeSymbolicLink) = "l"

instance FromJSON PathTypeW where
  parseJSON = Asn.withText "PathType" $ \case
    "f" -> pure $ MkPathTypeW PathTypeFile
    "d" -> pure $ MkPathTypeW PathTypeDirectory
    "l" -> pure $ MkPathTypeW PathTypeSymbolicLink
    other -> fail $ "Expected one of (f|d|l), received: " <> T.unpack other

instance Serialise PathTypeW where
  encode (MkPathTypeW PathTypeFile) = Serialise.encode @Char 'f'
  encode (MkPathTypeW PathTypeDirectory) = Serialise.encode @Char 'd'
  encode (MkPathTypeW PathTypeSymbolicLink) = Serialise.encode @Char 'l'

  decode = do
    c <- Serialise.decode @Char
    case c of
      'f' -> pure $ MkPathTypeW PathTypeFile
      'd' -> pure $ MkPathTypeW PathTypeDirectory
      'l' -> pure $ MkPathTypeW PathTypeSymbolicLink
      other -> fail $ "Expected (f|d|l), received: " ++ [other]

-- | This function tests both existence __and__ that the the path is of the
-- specified type.
existsFn ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  PathTypeW ->
  OsPath ->
  m Bool
existsFn (MkPathTypeW PathTypeFile) = doesFileExist
existsFn (MkPathTypeW PathTypeDirectory) = doesDirectoryExist
existsFn (MkPathTypeW PathTypeSymbolicLink) = doesSymbolicLinkExist

renameFn ::
  ( HasCallStack,
    MonadPathWriter m
  ) =>
  PathTypeW ->
  OsPath ->
  OsPath ->
  m ()
renameFn (MkPathTypeW PathTypeFile) = renameFile
renameFn (MkPathTypeW PathTypeDirectory) = renameDirectory
renameFn (MkPathTypeW PathTypeSymbolicLink) = PW.renamePath

deleteFn ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathTypeW ->
  OsPath ->
  m ()
deleteFn (MkPathTypeW PathTypeFile) = removeFile
deleteFn (MkPathTypeW PathTypeDirectory) = removeDirectoryRecursive
deleteFn (MkPathTypeW PathTypeSymbolicLink) = PW.removeSymbolicLink

copyPath ::
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathTypeW ->
  -- | Old path to copy.
  OsPath ->
  -- | Fully qualified name for file copy.
  OsPath ->
  -- | The directory in which to copy, for a dir copy.
  OsPath ->
  m ()
copyPath (MkPathTypeW PathTypeFile) old newName _ = PW.copyFileWithMetadata old newName
copyPath (MkPathTypeW PathTypeSymbolicLink) old newName _ = PW.copySymbolicLink old newName
copyPath (MkPathTypeW PathTypeDirectory) old _ newDir =
  PW.copyDirectoryRecursiveConfig copyConfig old newDir
  where
    copyConfig =
      MkCopyDirConfig
        { -- Need OverwriteDirectories because we may be performing multiple
          -- writes to the same dir (e.g. consider copying paths to a new
          -- trash dir during convert).
          overwrite = OverwriteDirectories,
          targetName = TargetNameSrc
        }
