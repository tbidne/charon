-- | Provides types.
module SafeRm.Data.PathType
  ( PathType (..),
    deleteFn,
    existsFn,
    renameFn,
  )
where

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
  ( PathReaderDynamic :> es
  ) =>
  PathType ->
  OsPath ->
  Eff es Bool
existsFn PathTypeFile = doesFileExist
existsFn PathTypeDirectory = doesDirectoryExist

renameFn ::
  ( PathWriterDynamic :> es
  ) =>
  PathType ->
  OsPath ->
  OsPath ->
  Eff es ()
renameFn PathTypeFile = renameFile
renameFn PathTypeDirectory = renameDirectory

deleteFn ::
  ( PathWriterDynamic :> es
  ) =>
  PathType ->
  OsPath ->
  Eff es ()
deleteFn PathTypeFile = removeFile
deleteFn PathTypeDirectory = removeDirectoryRecursive
