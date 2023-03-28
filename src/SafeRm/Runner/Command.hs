{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
--
-- @since 0.1
module SafeRm.Runner.Command
  ( -- * Types
    Command (..),
    CommandP1,
    CommandP2,

    -- * Optics
    _Delete,
    _DeletePerm,
    _Empty,
    _Restore,
    _List,
    _Metadata,
  )
where

import SafeRm.Data.Paths (PathI, PathIndex (TrashEntryFileName, TrashEntryOriginalPath))
import SafeRm.Data.UniqueSeq (UniqueSeq)
import SafeRm.Prelude
import SafeRm.Runner.Command.List (ListCmd)
import SafeRm.Runner.Phase (AdvancePhase (..), Phase (..))

instance AdvancePhase (Command Phase1) where
  type NextPhase (Command Phase1) = Command Phase2

  advancePhase (Delete paths) = Delete paths
  advancePhase (DeletePerm force paths) = DeletePerm force paths
  advancePhase (Empty b) = Empty b
  advancePhase (Restore paths) = Restore paths
  advancePhase Metadata = Metadata
  advancePhase (List cfg) = List $ advancePhase cfg

-- | Action to run.
--
-- @since 0.1
type Command :: Phase -> Type
data Command s
  = -- | Deletes a path.
    --
    -- @since 0.1
    Delete !(UniqueSeq (PathI TrashEntryOriginalPath))
  | -- | Permanently deletes a path from the trash.
    --
    -- @since 0.1
    DeletePerm
      !Bool
      !(UniqueSeq (PathI TrashEntryFileName))
  | -- | Empties the trash.
    --
    -- @since 0.1
    Empty !Bool
  | -- | Restores a path.
    --
    -- @since 0.1
    Restore (UniqueSeq (PathI TrashEntryFileName))
  | -- | List all trash contents.
    --
    -- @since 0.1
    List !(ListCmd s)
  | -- | Prints trash metadata.
    --
    -- @since 0.1
    Metadata

-- | @since 0.1
makePrisms ''Command

-- | @since 0.1
deriving stock instance Eq (Command Phase1)

-- | @since 0.1
deriving stock instance Show (Command Phase1)

-- | @since 0.1
deriving stock instance Eq (Command Phase2)

-- | @since 0.1
deriving stock instance Show (Command Phase2)

-- | @since 0.1
type CommandP1 = Command Phase1

-- | @since 0.1
type CommandP2 = Command Phase2
