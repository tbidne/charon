{-# LANGUAGE CPP #-}

-- | Provides exceptions used by SafeRm.
--
-- @since 0.1
module SafeRm.Exception
  ( -- * General
    PathNotFoundE (..),

    -- * Trash

    -- ** Entries

    -- *** General
    TrashEntryNotFoundE (..),

    -- *** Partial success
    TrashEntryPathNotFoundE (..),
    TrashEntryInfoNotFoundE (..),

    -- ** Directories
    TrashDirPathsNotFoundE (..),
    TrashDirInfoNotFoundE (..),

    -- * Misc
    RenameDuplicateE (..),
    RestoreCollisionE (..),
    RootE (..),
    EmptyPathE (..),
    InfoDecodeE (..),
  )
where

import Data.ByteString.Char8 qualified as Char8
import SafeRm.Data.Paths
  ( PathI,
    PathIndex (..),
  )
import SafeRm.Env qualified as Env
import SafeRm.Prelude

-- | Path is not found.
--
-- @since 0.1
newtype PathNotFoundE = MkPathNotFoundE FilePath
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception PathNotFoundE where
  displayException (MkPathNotFoundE f) =
    mconcat
      [ "Path not found: '",
        f,
        "'"
      ]

-- | Could not rename file due to duplicate names.
--
-- @since 0.1
newtype RenameDuplicateE = MkRenameDuplicateE (PathI TrashPath)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n) =
    mconcat
      [ "Failed renaming duplicate file: ",
        n ^. #unPathI
      ]

-- | Trash path not found error. Distinct from 'TrashEntryPathNotFoundE' in that
-- the latter indicates that the entry exists in trash/info but not
-- trash/paths, whereas this exception is less specific i.e. we found nothing
-- in trash/info but did not look in trash/paths.
--
-- @since 0.1
data TrashEntryNotFoundE
  = MkTrashEntryNotFoundE
      !(PathI TrashName)
      !(PathI TrashInfoPath)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TrashEntryNotFoundE where
  displayException (MkTrashEntryNotFoundE name path) =
    mconcat
      [ "No entry for '",
        name ^. #unPathI,
        "'; did not find '",
        path ^. #unPathI,
        "'"
      ]

-- | Path found in trash/info but not trash/paths error.
--
-- @since 0.1
data TrashEntryPathNotFoundE
  = MkTrashEntryPathNotFoundE
      !(PathI TrashHome)
      !(PathI TrashName)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TrashEntryPathNotFoundE where
  displayException (MkTrashEntryPathNotFoundE thome name) =
    mconcat
      [ "The path '",
        name ^. #unPathI,
        "' was not found in '",
        thome ^. #unPathI,
        slash,
        "paths' despite being listed in '",
        thome ^. #unPathI,
        slash,
        "info'. This can be fixed by manually deleting the ",
        Env.trashInfoExtension,
        " file or deleting everything (i.e. sr e -f)."
      ]

-- | Path found in trash/paths but not trash/info error
--
-- @since 0.1
data TrashEntryInfoNotFoundE
  = MkTrashEntryInfoNotFoundE
      !(PathI TrashHome)
      !(PathI TrashName)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TrashEntryInfoNotFoundE where
  displayException (MkTrashEntryInfoNotFoundE thome name) =
    mconcat
      [ "The path '",
        name ^. #unPathI,
        Env.trashInfoExtension,
        "' was not found in '",
        thome ^. #unPathI,
        slash,
        "info' despite being listed in '",
        thome ^. #unPathI,
        slash,
        "paths'. This can be fixed by manually deleting the ",
        slash,
        "paths entry or deleting everything (i.e. sr e -f)."
      ]

-- | Trash path dir not found error.
--
-- @since 0.1
newtype TrashDirPathsNotFoundE = MkTrashDirPathsNotFoundE (PathI TrashHome)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TrashDirPathsNotFoundE where
  displayException (MkTrashDirPathsNotFoundE th) =
    mconcat
      [ "The trash paths directory was not found at '",
        th ^. #unPathI,
        slash,
        "paths' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. sr e -f)."
      ]

-- | Trash info dir not found error.
--
-- @since 0.1
newtype TrashDirInfoNotFoundE = MkTrashDirInfoNotFoundE (PathI TrashHome)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception TrashDirInfoNotFoundE where
  displayException (MkTrashDirInfoNotFoundE th) =
    mconcat
      [ "The trash info directory was not found at '",
        th ^. #unPathI,
        slash,
        "info' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. sr e -f)."
      ]

-- | Collision with existing file when attempting a restore.
--
-- @since 0.1
data RestoreCollisionE
  = MkRestoreCollisionE
      !(PathI TrashName)
      !(PathI OriginalPath)
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception RestoreCollisionE where
  displayException (MkRestoreCollisionE n o) =
    mconcat
      [ "Cannot restore the trash file '",
        n ^. #unPathI,
        "' as one exists at the original location: ",
        o ^. #unPathI
      ]

-- | Exception for deleting the root.
--
-- @since 0.1
data RootE = MkRootE
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception RootE where
  displayException _ = "Attempted to delete root! This is not allowed."

-- | Exception for deleting an empty path.
--
-- @since 0.1
data EmptyPathE = MkEmptyPathE
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception EmptyPathE where
  displayException _ = "Attempted to delete the empty path! This is not allowed."

-- | Exception for decoding.
--
-- @since 0.1
data InfoDecodeE = MkInfoDecodeE (PathI TrashInfoPath) ByteString String
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception InfoDecodeE where
  displayException (MkInfoDecodeE path bs err) =
    mconcat
      [ "Could not decode path '",
        path ^. #unPathI,
        "' contents\n",
        Char8.unpack bs,
        "\n: ",
        err
      ]

slash :: FilePath
#if WINDOWS
slash = "\\"
#else
slash = "/"
#endif
