{-# LANGUAGE CPP #-}

-- | Provides exceptions used by Charon.
module Charon.Exception
  ( -- * General
    SomethingWentWrong (..),

    -- * Trash

    -- ** Entries

    -- *** General
    TrashEntryNotFoundE (..),
    TrashEntryWildcardNotFoundE (..),

    -- *** Partial success
    TrashEntryFileNotFoundE (..),
    TrashEntryInfoNotFoundE (..),
    TrashEntryInfoBadExtE (..),

    -- * Illegal paths
    DotsPathE (..),
    EmptyPathE (..),
    RootE (..),
    TildePathE (..),

    -- * Trash name deriving
    FileNameEmptyE (..),
    RenameDuplicateE (..),
    UniquePathNotPrefixE (..),

    -- * Misc
    BackendDetectE (..),
    EmptySearchResults (..),
    InfoDecodeE (..),
    PathNotFound (..),
    RestoreCollisionE (..),
  )
where

import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend
import Charon.Data.Paths
  ( PathI (MkPathI),
    PathIndex
      ( TrashEntryFileName,
        TrashEntryInfo,
        TrashEntryOriginalPath,
        TrashEntryPath,
        TrashHome
      ),
  )
import Charon.Data.Paths qualified as Paths
import Charon.Data.UniqueSeq (UniqueSeq)
import Charon.Prelude
import GHC.Exts (IsList (toList))
import System.OsPath (encodeUtf)

-- NOTE: [Callstacks]
--
-- Callstacks are useful for diagnosing __unknown__ problems, but they are
-- fairly noisy, so they are nice to avoid when the problem is fully
-- understood. For example, if someone tries to delete an unknown path, the
-- error message alone is perfectly descriptive; no one is helped by
-- attaching a monstrously long exception. Thus we have arrived at the
-- following design:
--
-- 1. We do not need callstacks for exceptions explicitly thrown by Charon.
--    These are all self-explanatory, so we can easily figure out what went
--    wrong.
--
-- 2. Exceptions thrown by dependencies (i.e. ones that "got through") should
--    have callstacks attached by default, removing them on a case-by-case
--    basis.
--
-- To achieve this, we only use throwM on possibly unknown exceptions
-- (catchSync), and we use exceptions' displayCSNoMatch function to explicitly
-- opt-out of callstacks for our exceptions.

-- | Could not rename file due to duplicate names.
newtype RenameDuplicateE = MkRenameDuplicateE (PathI TrashEntryPath)
  deriving stock (Show)

instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n) =
    mconcat
      [ "Failed renaming duplicate file: '",
        decodeDisplayEx $ n ^. #unPathI,
        "'"
      ]

-- | Generic trash path not found error. Distinct from 'TrashEntryFileNotFoundE'
-- in that the latter indicates that the entry exists in @trash\/info@ but not
-- @trash\/files@, whereas this exception is less specific i.e. we found nothing
-- in @trash\/info@ but did not look in @trash\/files@.
newtype TrashEntryNotFoundE = MkTrashEntryNotFoundE (PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryNotFoundE where
  displayException (MkTrashEntryNotFoundE name) =
    mconcat
      [ "No entry for '",
        decodeDisplayEx $ name ^. #unPathI,
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
        decodeDisplayEx $ name ^. #unPathI,
        "'"
      ]

-- | Path found in the index but not files.
data TrashEntryFileNotFoundE
  = MkTrashEntryFileNotFoundE
      (PathI TrashHome)
      (PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryFileNotFoundE where
  displayException (MkTrashEntryFileNotFoundE (MkPathI thome) name) =
    mconcat
      [ "The file '",
        decodeDisplayEx $ name ^. #unPathI,
        "' was not found in the trash '",
        decodeDisplayEx thome,
        "' despite being listed in the index. This can be ",
        "fixed by manually deleting the info file or deleting everything ",
        "(i.e. charon empty -f)."
      ]

-- | Path found files but not index.
data TrashEntryInfoNotFoundE
  = MkTrashEntryInfoNotFoundE
      (PathI TrashHome)
      (PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception TrashEntryInfoNotFoundE where
  displayException (MkTrashEntryInfoNotFoundE (MkPathI thome) (MkPathI name)) =
    mconcat
      [ "The file '",
        decodeDisplayEx nameExt,
        "' was not found in the trash '",
        decodeDisplayEx thome,
        "' index despite existing in the trash itself. This can be fixed by ",
        "manually deleting the entry or deleting everything ",
        "(i.e. charon empty -f)."
      ]
    where
      -- Have to do this because ".<ext>" is not valid on windows i.e. fails
      -- QuasiQuote
      nameExt = case encodeUtf ".<ext>" of
        Nothing -> name
        Just s -> name <> s

-- | Unexpected file extension error.
data TrashEntryInfoBadExtE
  = MkTrashEntryInfoBadExtE
      (PathI TrashEntryFileName)
      OsPath
      OsPath
  deriving stock (Show)

instance Exception TrashEntryInfoBadExtE where
  displayException (MkTrashEntryInfoBadExtE name actualExt expectedExt) =
    mconcat
      [ "The trash index file '",
        decodeDisplayEx $ name ^. #unPathI,
        "' has an unexpected file extension: '",
        decodeDisplayEx actualExt,
        "'. Expected '",
        decodeDisplayEx expectedExt,
        "'"
      ]

-- | Unexpected backend error.
newtype BackendDetectE = MkBackendDetectE Backend
  deriving stock (Show)

instance Exception BackendDetectE where
  displayException (MkBackendDetectE wantedExt) =
    mconcat
      [ "Wanted backend '",
        Backend.backendName wantedExt,
        "', but detected something else"
      ]

-- | Collision with existing file when attempting a restore.
data RestoreCollisionE
  = MkRestoreCollisionE
      (PathI TrashEntryFileName)
      (PathI TrashEntryOriginalPath)
  deriving stock (Show)

instance Exception RestoreCollisionE where
  displayException (MkRestoreCollisionE n o) =
    mconcat
      [ "Cannot restore the trash file '",
        decodeDisplayEx $ n ^. #unPathI,
        "' as one exists at the original location: '",
        decodeDisplayEx $ o ^. #unPathI,
        "'"
      ]

-- | Exception for paths with a tilde.
newtype TildePathE = MkTildePathE (PathI TrashEntryOriginalPath)
  deriving stock (Show)

instance Exception TildePathE where
  displayException (MkTildePathE p) =
    mconcat
      [ "Attempted to delete path with a tilde! This is not allowed: ",
        decodeDisplayEx $ p ^. #unPathI
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

-- | Exception for deleting the special dots paths.
newtype DotsPathE = MkDotsPathE (PathI TrashEntryOriginalPath)
  deriving stock (Show)

instance Exception DotsPathE where
  displayException (MkDotsPathE p) =
    mconcat
      [ "Attempted to delete the special path '",
        decodeDisplayEx $ p ^. #unPathI,
        "'! This is not allowed."
      ]

-- | Exception for deriving an empty file name.
newtype FileNameEmptyE = MkFileNameEmptyE (PathI TrashEntryOriginalPath)
  deriving stock (Show)

instance Exception FileNameEmptyE where
  displayException (MkFileNameEmptyE p) =
    mconcat
      [ "Derived empty file name from the path '",
        decodeDisplayEx $ p ^. #unPathI,
        "'"
      ]

-- | Exception for when the original name is not a prefix of the derived
-- unique name.
data UniquePathNotPrefixE
  = MkUniquePathNotPrefixE
      (PathI TrashEntryFileName)
      (PathI TrashEntryFileName)
  deriving stock (Show)

instance Exception UniquePathNotPrefixE where
  displayException (MkUniquePathNotPrefixE origName newName) =
    mconcat
      [ "Original path name '",
        decodeDisplayEx $ origName ^. #unPathI,
        "' is not a prefix of the new unique name '",
        decodeDisplayEx $ newName ^. #unPathI,
        "'"
      ]

-- | Exception for decoding.
data InfoDecodeE = MkInfoDecodeE (PathI TrashEntryInfo) ByteString String
  deriving stock (Show)

instance Exception InfoDecodeE where
  displayException (MkInfoDecodeE path bs err) =
    mconcat
      [ "Could not decode file '",
        decodeDisplayEx $ path ^. #unPathI,
        "' with contents:\n",
        bsToStrLenient bs,
        "\nError: ",
        err
      ]

-- | No search results.
newtype EmptySearchResults
  = MkEmptySearchResults (UniqueSeq (PathI TrashEntryFileName))
  deriving stock (Show)

instance Exception EmptySearchResults where
  displayException (MkEmptySearchResults useq) =
    mconcat
      [ "Search for paths failed: ",
        Paths.showPaths $ toList (useq ^. #seq)
      ]

-- | General exception for a missing path.
newtype PathNotFound = MkPathNotFound OsPath
  deriving stock (Show)

instance Exception PathNotFound where
  displayException (MkPathNotFound p) =
    mconcat
      [ "Path not found: '",
        decodeDisplayEx p,
        "'"
      ]

-- | General error for something going wrong. Used as the final "overall"
-- error for when some code can catch multiple exceptions.
data SomethingWentWrong = MkSomethingWentWrong

deriving stock instance Show SomethingWentWrong

instance Exception SomethingWentWrong where
  displayException _ = "Something went wrong."
