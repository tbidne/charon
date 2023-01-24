{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
--
-- @since 0.1
module SafeRm.Runner.Command
  ( -- * Types
    Command (..),
    ListCommand (..),

    -- * Optics
    _Delete,
    _DeletePerm,
    _Empty,
    _Restore,
    _List,
    _Metadata,
  )
where

import SafeRm.Data.Index (Sort)
import SafeRm.Data.PathData (PathDataFormat)
import SafeRm.Data.Paths (PathI, PathIndex (OriginalPath, TrashName))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Prelude

-- | Arguments for the list command.
--
-- @since 0.1
data ListCommand = MkListCommand
  { -- | Format style.
    --
    -- @since 0.1
    format :: !PathDataFormat,
    -- | How to sort the list.
    --
    -- @since 0.1
    sort :: !Sort,
    -- | Whether to reverse the sort.
    --
    -- @since 0.1
    revSort :: !Bool
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''ListCommand

-- | Action to run.
--
-- @since 0.1
data Command
  = -- | Deletes a path.
    --
    -- @since 0.1
    Delete !(UniqueSeq (PathI OriginalPath))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    DeletePerm
      !Bool
      !(UniqueSeq (PathI TrashName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    Empty !Bool
  | -- | Restores a path.
    --
    -- @since 0.1
    Restore (UniqueSeq (PathI TrashName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    List !ListCommand
  | -- | Prints trash metadata.
    --
    -- @since 0.1
    Metadata
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makePrisms ''Command
