-- | Provides types.
module SafeRm.Data.PathType
  ( PathType (..),
    deleteFn,
    existsFn,
    renameFn,
    copyPath,
  )
where

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
import SafeRm.Data.Serialize (Serialize (DecodeExtra, decode, encode))
import SafeRm.Prelude

-- | Path type.
data PathType
  = -- | File type
    PathTypeFile
  | -- | Directory type
    PathTypeDirectory
  | -- | Symbolic link
    PathTypeSymlink
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

instance Pretty PathType where
  pretty PathTypeFile = "File"
  pretty PathTypeDirectory = "Directory"
  pretty PathTypeSymlink = "Symlink"

instance ToJSON PathType where
  toJSON PathTypeFile = "f"
  toJSON PathTypeDirectory = "d"
  toJSON PathTypeSymlink = "s"

instance FromJSON PathType where
  parseJSON = Asn.withText "PathType" $ \case
    "f" -> pure PathTypeFile
    "d" -> pure PathTypeDirectory
    "s" -> pure PathTypeSymlink
    other -> fail $ "Expected one of (f|d|s), received: " <> T.unpack other

instance Serialise PathType where
  encode PathTypeFile = Serialise.encode @Char 'f'
  encode PathTypeDirectory = Serialise.encode @Char 'd'
  encode PathTypeSymlink = Serialise.encode @Char 's'

  decode = do
    c <- Serialise.decode @Char
    case c of
      'f' -> pure PathTypeFile
      'd' -> pure PathTypeDirectory
      's' -> pure PathTypeSymlink
      other -> fail $ "Expected (f|d|s), received: " ++ [other]

instance Serialize PathType where
  type DecodeExtra PathType = ()
  encode PathTypeFile = pure "f"
  encode PathTypeDirectory = pure "d"
  encode PathTypeSymlink = pure "s"

  decode _ "f" = Right PathTypeFile
  decode _ "d" = Right PathTypeDirectory
  decode _ "s" = Right PathTypeSymlink
  decode _ other = Left $ "Could not decode path type: '" <> bsToStr other <> "'"

-- | This function tests both existence __and__ that the the path is of the
-- specified type.
existsFn ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m
  ) =>
  PathType ->
  OsPath ->
  m Bool
existsFn PathTypeFile = doesFileExist
existsFn PathTypeDirectory = doesDirectoryExist
existsFn PathTypeSymlink = doesSymbolicLinkExist

renameFn ::
  ( HasCallStack,
    MonadPathWriter m
  ) =>
  PathType ->
  OsPath ->
  OsPath ->
  m ()
renameFn PathTypeFile = renameFile
renameFn PathTypeDirectory = renameDirectory
renameFn PathTypeSymlink = PW.renamePath

deleteFn ::
  ( HasCallStack,
    MonadPathWriter m
  ) =>
  PathType ->
  OsPath ->
  m ()
deleteFn PathTypeFile = removeFile
deleteFn PathTypeDirectory = removeDirectoryRecursive
-- FIXME: distinguish between file/dir links for windows
deleteFn PathTypeSymlink = PW.removeDirectoryLink

copyPath ::
  ( HasCallStack,
    MonadIORef m,
    MonadMask m,
    MonadPathReader m,
    MonadPathWriter m
  ) =>
  PathType ->
  -- | Old path to copy.
  OsPath ->
  -- | Fully qualified name for file copy.
  OsPath ->
  -- | The directory in which to copy, for a dir copy.
  OsPath ->
  m ()
copyPath PathTypeFile old newName _ = PW.copyFileWithMetadata old newName
copyPath PathTypeSymlink old newName _ = PW.copySymbolicLink old newName
copyPath PathTypeDirectory old _ newDir =
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
