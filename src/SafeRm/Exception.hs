{-# LANGUAGE CPP #-}

-- | Provides exceptions used by SafeRm.
module SafeRm.Exception
  ( -- * Trash

    -- ** Entries

    -- *** General
    TrashEntryNotFoundE (..),
    TrashEntryWildcardNotFoundE (..),

    -- *** Partial success
    TrashEntryFileNotFoundE (..),
    TrashEntryInfoNotFoundE (..),
    TrashEntryInfoBadExtE (..),

    -- * Misc
    RenameDuplicateE (..),
    RestoreCollisionE (..),
    RootE (..),
    EmptyPathE (..),
    DotsPathE (..),
    InfoDecodeE (..),
    EmptySearchResults (..),
  )
where

import GHC.Exts (IsList (toList))
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
import SafeRm.Data.Paths qualified as Paths
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Prelude
import System.OsPath (encodeUtf)

-- REVIEW: Consider making some (all?) of these IOException. This would be
-- less structured, but it would match most of the exceptions thrown by our
-- deps (e.g. directory).
--
-- If we did this, we would have to make sure tests check the type/message
-- to ensure we're throwing the correct one.

-- | Could not rename file due to duplicate names.
newtype RenameDuplicateE = MkRenameDuplicateE (PathI TrashEntryPath)
  deriving stock (Show)

instance Exception RenameDuplicateE where
  displayException (MkRenameDuplicateE n) =
    mconcat
      [ "Failed renaming duplicate file: '",
        decodeOsToFpDisplayEx $ n ^. #unPathI,
        "'"
      ]

-- | Trash path not found error. Distinct from 'TrashEntryFileNotFoundE' in that
-- the latter indicates that the entry exists in @trash\/info@ but not
-- @trash\/files@, whereas this exception is less specific i.e. we found nothing
-- in @trash\/info@ but did not look in @trash\/files@.
data TrashEntryNotFoundE
  = MkTrashEntryNotFoundE
      (PathI TrashEntryFileName)
      (PathI TrashEntryInfo)
  deriving stock (Show)

instance Exception TrashEntryNotFoundE where
  displayException (MkTrashEntryNotFoundE name path) =
    mconcat
      [ "No entry for '",
        decodeOsToFpDisplayEx $ name ^. #unPathI,
        "'; did not find index file '",
        decodeOsToFpDisplayEx $ path ^. #unPathI,
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
        decodeOsToFpDisplayEx $ name ^. #unPathI,
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
        decodeOsToFpDisplayEx $ name ^. #unPathI,
        "' was not found in the trash '",
        decodeOsToFpDisplayEx thome,
        "' despite being listed in the index. This can be ",
        "fixed by manually deleting the info file or deleting everything ",
        "(i.e. safe-rm empty -f)."
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
        decodeOsToFpDisplayEx nameExt,
        "' was not found in the trash '",
        decodeOsToFpDisplayEx thome,
        "' index despite existing in the trash itself. This can be fixed by ",
        "manually deleting the entry or deleting everything ",
        "(i.e. safe-rm empty -f)."
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
        decodeOsToFpDisplayEx $ name ^. #unPathI,
        "' has an unexpected file extension: '",
        decodeOsToFpDisplayEx actualExt,
        "'. Expected '",
        decodeOsToFpDisplayEx expectedExt,
        "'"
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
        decodeOsToFpDisplayEx $ n ^. #unPathI,
        "' as one exists at the original location: '",
        decodeOsToFpDisplayEx $ o ^. #unPathI,
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

-- | Exception for deleting the special dots paths.
newtype DotsPathE = MkDotsPathE (PathI TrashEntryOriginalPath)
  deriving stock (Show)

instance Exception DotsPathE where
  displayException (MkDotsPathE p) =
    mconcat
      [ "Attempted to delete the special path '",
        decodeOsToFpDisplayEx $ p ^. #unPathI,
        "'! This is not allowed."
      ]

-- | Exception for decoding.
data InfoDecodeE = MkInfoDecodeE (PathI TrashEntryInfo) ByteString String
  deriving stock (Show)

instance Exception InfoDecodeE where
  displayException (MkInfoDecodeE path bs err) =
    mconcat
      [ "Could not decode file '",
        decodeOsToFpDisplayEx $ path ^. #unPathI,
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
