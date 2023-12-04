{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
module Charon.Runner.Command
  ( -- * Types
    Command (..),
    CommandP1,
    CommandP2,

    -- * Optics
    _Delete,
    _PermDelete,
    _Empty,
    _Restore,
    _List,
    _Metadata,
  )
where

import Charon.Backend.Data (Backend)
import Charon.Data.Paths
  ( PathI,
    PathIndex
      ( TrashEntryFileName,
        TrashEntryOriginalPath,
        TrashHome
      ),
  )
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Prelude
import Charon.Runner.Command.List (ListCmd)
import Charon.Runner.Phase (AdvancePhase (..), Phase (..))

instance AdvancePhase (Command Phase1) where
  type NextPhase (Command Phase1) = Command Phase2

  advancePhase (Delete paths) = Delete paths
  advancePhase (PermDelete force paths) = PermDelete force paths
  advancePhase (Empty b) = Empty b
  advancePhase (Restore paths) = Restore paths
  advancePhase Metadata = Metadata
  advancePhase (List cfg) = List $ advancePhase cfg
  advancePhase (Convert dest) = Convert dest
  advancePhase (Merge dest) = Merge dest

-- | Action to run.
type Command :: Phase -> Type
data Command s
  = -- | Deletes a path.
    Delete (UniqueSeqNE (PathI TrashEntryOriginalPath))
  | -- | Permanently deletes a path from the trash.
    PermDelete
      Bool
      (UniqueSeqNE (PathI TrashEntryFileName))
  | -- | Empties the trash.
    Empty Bool
  | -- | Restores a path.
    Restore (UniqueSeqNE (PathI TrashEntryFileName))
  | -- | List all trash contents.
    List (ListCmd s)
  | -- | Prints trash metadata.
    Metadata
  | -- | Converts backend files.
    Convert Backend
  | -- | Merges trash home directories.
    Merge (PathI TrashHome)

makePrisms ''Command

deriving stock instance Eq (Command Phase1)

deriving stock instance Show (Command Phase1)

deriving stock instance Eq (Command Phase2)

deriving stock instance Show (Command Phase2)

type CommandP1 = Command Phase1

type CommandP2 = Command Phase2
