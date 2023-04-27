{-# LANGUAGE CPP #-}

-- | Provides exceptions used by SafeRm.
module SafeRm.Exception
  ( -- * General
    FileNotFoundE (..),

    -- * Trash

    -- ** Entries

    -- *** General
    TrashEntryNotFoundE (..),
    TrashEntryWildcardNotFoundE (..),

    -- *** Partial success
    TrashEntryFileNotFoundE (..),
    TrashEntryInfoNotFoundE (..),
    TrashEntryInfoBadExtE (..),

    -- ** Directories
    TrashDirFilesNotFoundE (..),
    TrashDirInfoNotFoundE (..),

    -- * Misc
    RenameDuplicateE (..),
    RestoreCollisionE (..),
    RootE (..),
    EmptyPathE (..),
    InfoDecodeE (..),
    PathNotFileDirE (..),
  )
where

import SafeRm.Data.Paths (PathI, PathIndex (..))
import SafeRm.Env qualified as Env
import SafeRm.Prelude

-- | Path is not found.
newtype FileNotFoundE = MkFileNotFoundE FilePath
  deriving stock (Show)

instance Exception FileNotFoundE where
  displayException (MkFileNotFoundE f) =
    mconcat
      [ "File not found: '",
        f,
        "'"
      ]

-- | Could not rename file due to duplicate names.
newtype RenameDuplicateE = MkRenameDuplicateE (PathI TrashEntryPath)
  deriving stock (Show)

instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n) =
    mconcat
      [ "Failed renaming duplicate file: ",
        n ^. #unPathI
      ]

-- | Trash path not found error. Distinct from 'TrashEntryFileNotFoundE' in that
-- the latter indicates that the entry exists in @trash\/info@ but not
-- @trash\/files@, whereas this exception is less specific i.e. we found nothing
-- in @trash\/info@ but did not look in @trash\/files@.
data TrashEntryNotFoundE
  = MkTrashEntryNotFoundE
      !(PathI TrashEntryFileName)
      !(PathI TrashEntryInfo)
  deriving stock (Show)

instance Exception TrashEntryNotFoundE where
  displayException (MkTrashEntryNotFoundE name path) =
    mconcat
      [ "No entry for '",
        name ^. #unPathI,
        "'; did not find '",
        path ^. #unPathI,
        "'"
      ]

-- | Error for not finding any files via wildcard search.
newtype TrashEntryWildcardNotFoundE
  = MkTrashEntryWildcardNotFoundE (PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryWildcardNotFoundE where
  displayException (MkTrashEntryWildcardNotFoundE name) =
    mconcat
      [ "No entries found for wildcard search '",
        name ^. #unPathI,
        "'"
      ]

-- | Path found in @trash\/info@ but not @trash\/files@ error.
data TrashEntryFileNotFoundE
  = MkTrashEntryFileNotFoundE
      !(PathI TrashHome)
      !(PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryFileNotFoundE where
  displayException (MkTrashEntryFileNotFoundE thome name) =
    mconcat
      [ "The file '",
        name ^. #unPathI,
        "' was not found in '",
        thome ^. #unPathI,
        slash,
        "files' despite being listed in '",
        thome ^. #unPathI,
        slash,
        "info'. This can be fixed by manually deleting the ",
        Env.trashInfoExtension,
        " file or deleting everything (i.e. safe-rm empty -f)."
      ]

-- | Path found in @trash\/files@ but not @trash\/info@ error.
data TrashEntryInfoNotFoundE
  = MkTrashEntryInfoNotFoundE
      !(PathI TrashHome)
      !(PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryInfoNotFoundE where
  displayException (MkTrashEntryInfoNotFoundE thome name) =
    mconcat
      [ "The file '",
        name ^. #unPathI,
        Env.trashInfoExtension,
        "' was not found in '",
        thome ^. #unPathI,
        slash,
        "info' despite being listed in '",
        thome ^. #unPathI,
        slash,
        "files'. This can be fixed by manually deleting the ",
        slash,
        "files entry or deleting everything (i.e. safe-rm empty -f)."
      ]

-- | Path found in @trash\/files@ but not @trash\/info@ error.
data TrashEntryInfoBadExtE
  = MkTrashEntryInfoBadExtE
      !(PathI TrashEntryFileName)
      !FilePath
  deriving stock (Show)

instance Exception TrashEntryInfoBadExtE where
  displayException (MkTrashEntryInfoBadExtE name ext) =
    mconcat
      [ "The trash info file '",
        name ^. #unPathI,
        "' has an unexpected file extension: ' ",
        ext,
        "'. Expected '",
        Env.trashInfoExtension,
        "'"
      ]

-- | Trash path dir not found error.
newtype TrashDirFilesNotFoundE = MkTrashDirFilesNotFoundE (PathI TrashHome)
  deriving stock (Show)

instance Exception TrashDirFilesNotFoundE where
  displayException (MkTrashDirFilesNotFoundE th) =
    mconcat
      [ "The trash files directory was not found at '",
        th ^. #unPathI,
        slash,
        "files' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]

-- | Trash info dir not found error.
newtype TrashDirInfoNotFoundE = MkTrashDirInfoNotFoundE (PathI TrashHome)
  deriving stock (Show)

instance Exception TrashDirInfoNotFoundE where
  displayException (MkTrashDirInfoNotFoundE th) =
    mconcat
      [ "The trash info directory was not found at '",
        th ^. #unPathI,
        slash,
        "info' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]

-- | Collision with existing file when attempting a restore.
data RestoreCollisionE
  = MkRestoreCollisionE
      !(PathI TrashEntryFileName)
      !(PathI TrashEntryOriginalPath)
  deriving stock (Show)

instance Exception RestoreCollisionE where
  displayException (MkRestoreCollisionE n o) =
    mconcat
      [ "Cannot restore the trash file '",
        n ^. #unPathI,
        "' as one exists at the original location: ",
        o ^. #unPathI
      ]

-- | Exception for deleting the root.
data RootE = MkRootE
  deriving stock (Show)

instance Exception RootE where
  displayException _ = "Attempted to delete root! This is not allowed."

-- | Exception for deleting an empty path.
data EmptyPathE = MkEmptyPathE
  deriving stock (Show)

instance Exception EmptyPathE where
  displayException _ = "Attempted to delete the empty path! This is not allowed."

-- | Exception for decoding.
data InfoDecodeE = MkInfoDecodeE (PathI TrashEntryInfo) ByteString String
  deriving stock (Show)

instance Exception InfoDecodeE where
  displayException (MkInfoDecodeE path bs err) =
    mconcat
      [ "Could not decode file '",
        path ^. #unPathI,
        "' with contents:\n",
        bsToStrLenient bs,
        "\nError: ",
        err
      ]

-- | Could not rename file due to duplicate names.
newtype PathNotFileDirE = MkPathNotFileDirE FilePath
  deriving stock (Show)

instance Exception PathNotFileDirE where
  displayException (MkPathNotFileDirE p) =
    mconcat
      [ "Path exists but is not a file or directory: '",
        p,
        "'"
      ]

slash :: FilePath
#if WINDOWS
slash = "\\"
#else
slash = "/"
#endif
