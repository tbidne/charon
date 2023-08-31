{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

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

import Effects.FileSystem.Utils qualified as FsUtils
import SafeRm.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryInfo,
        TrashEntryOriginalPath,
        TrashEntryPath,
        TrashHome
      ),
  )
import SafeRm.Prelude
import System.OsPath (encodeUtf)

-- | Path is not found.
newtype FileNotFoundE = MkFileNotFoundE OsPath
  deriving stock (Show)

instance Exception FileNotFoundE where
  displayException (MkFileNotFoundE f) =
    mconcat
      [ "File not found: ",
        FsUtils.osToFp f
      ]

-- | Could not rename file due to duplicate names.
newtype RenameDuplicateE = MkRenameDuplicateE (PathI TrashEntryPath)
  deriving stock (Show)

instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n) =
    mconcat
      [ "Failed renaming duplicate file: ",
        FsUtils.osToFp $ n ^. #unPathI
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
        FsUtils.osToFp $ name ^. #unPathI,
        "'; did not find '",
        FsUtils.osToFp $ path ^. #unPathI,
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
        FsUtils.osToFp $ name ^. #unPathI,
        "'"
      ]

-- | Path found in @trash\/info@ but not @trash\/files@ error.
data TrashEntryFileNotFoundE
  = MkTrashEntryFileNotFoundE
      !(PathI TrashHome)
      !(PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryFileNotFoundE where
  displayException (MkTrashEntryFileNotFoundE (MkPathI thome) name) =
    mconcat
      [ "The file '",
        FsUtils.osToFp $ name ^. #unPathI,
        "' was not found in '",
        FsUtils.osToFp filesPath,
        "' despite being listed in '",
        FsUtils.osToFp infoPath,
        "'. This can be fixed by manually deleting the info file or ",
        "deleting everything (i.e. safe-rm empty -f)."
      ]
    where
      filesPath = thome </> pathFiles
      infoPath = thome </> pathInfo

-- | Path found in @trash\/files@ but not @trash\/info@ error.
data TrashEntryInfoNotFoundE
  = MkTrashEntryInfoNotFoundE
      !(PathI TrashHome)
      !(PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryInfoNotFoundE where
  displayException (MkTrashEntryInfoNotFoundE (MkPathI thome) (MkPathI name)) =
    mconcat
      [ "The file '",
        FsUtils.osToFp nameExt,
        "' was not found in '",
        FsUtils.osToFp infoPath,
        "' despite being listed in '",
        FsUtils.osToFp filesPath,
        "'. This can be fixed by manually deleting the 'files' ",
        "entry or deleting everything (i.e. safe-rm empty -f)."
      ]
    where
      -- Have to do this because ".<ext>" is not valid on windows i.e. fails
      -- QuasiQuote
      nameExt = case encodeUtf ".<ext>" of
        Nothing -> name
        Just s -> name <> s
      filesPath = thome </> pathFiles
      infoPath = thome </> pathInfo

-- | Path found in @trash\/files@ but not @trash\/info@ error.
data TrashEntryInfoBadExtE
  = MkTrashEntryInfoBadExtE
      !(PathI TrashEntryFileName)
      !String
      !String
  deriving stock (Show)

instance Exception TrashEntryInfoBadExtE where
  displayException (MkTrashEntryInfoBadExtE name actualExt expectedExt) =
    mconcat
      [ "The trash info file '",
        FsUtils.osToFp $ name ^. #unPathI,
        "' has an unexpected file extension: '",
        actualExt,
        "'. Expected '",
        expectedExt,
        "'"
      ]

-- | Trash path dir not found error.
newtype TrashDirFilesNotFoundE = MkTrashDirFilesNotFoundE (PathI TrashHome)
  deriving stock (Show)

instance Exception TrashDirFilesNotFoundE where
  displayException (MkTrashDirFilesNotFoundE (MkPathI th)) =
    mconcat
      [ "The trash files directory was not found at '",
        FsUtils.osToFp files,
        "' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]
    where
      files = th </> pathFiles

-- | Trash info dir not found error.
newtype TrashDirInfoNotFoundE = MkTrashDirInfoNotFoundE (PathI TrashHome)
  deriving stock (Show)

instance Exception TrashDirInfoNotFoundE where
  displayException (MkTrashDirInfoNotFoundE (MkPathI th)) =
    mconcat
      [ "The trash info directory was not found at '",
        FsUtils.osToFp info,
        "' despite the trash home existing. This can be fixed by ",
        "manually creating the directory or resetting everything (i.e. safe-rm empty -f)."
      ]
    where
      info = th </> [osp|info|]

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
        FsUtils.osToFp $ n ^. #unPathI,
        "' as one exists at the original location: '",
        FsUtils.osToFp $ o ^. #unPathI,
        "'"
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
        FsUtils.osToFp $ path ^. #unPathI,
        "' with contents:\n",
        bsToStrLenient bs,
        "\nError: ",
        err
      ]

-- | Could not rename file due to duplicate names.
newtype PathNotFileDirE = MkPathNotFileDirE OsPath
  deriving stock (Show)

instance Exception PathNotFileDirE where
  displayException (MkPathNotFileDirE p) =
    mconcat
      [ "Path exists but is not a file or directory: '",
        FsUtils.osToFp p,
        "'"
      ]
