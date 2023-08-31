-- | Provides types.
module SafeRm.Data.PathType
  ( PathType (..),
    deleteFn,
    existsFn,
    renameFn,
  )
where

import Effects.FileSystem.PathWriter (MonadPathWriter (removeFile))
import SafeRm.Data.Serialize (Serialize (DecodeExtra, decode, encode))
import SafeRm.Prelude

-- | Path type.
data PathType
  = -- | File type.
    PathTypeFile
  | -- | Directory type
    PathTypeDirectory
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

instance Pretty PathType where
  pretty PathTypeFile = "File"
  pretty PathTypeDirectory = "Directory"

instance Serialize PathType where
  type DecodeExtra PathType = ()
  encode PathTypeFile = pure "f"
  encode PathTypeDirectory = pure "d"

  decode _ "f" = Right PathTypeFile
  decode _ "d" = Right PathTypeDirectory
  decode _ other = Left $ "Could not decode path type: '" <> bsToStr other <> "'"

existsFn ::
  ( HasCallStack,
    MonadPathReader m
  ) =>
  PathType ->
  OsPath ->
  m Bool
existsFn PathTypeFile = doesFileExist
existsFn PathTypeDirectory = doesDirectoryExist

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

deleteFn ::
  ( HasCallStack,
    MonadPathWriter m
  ) =>
  PathType ->
  OsPath ->
  m ()
deleteFn PathTypeFile = removeFile
deleteFn PathTypeDirectory = removeDirectoryRecursive
