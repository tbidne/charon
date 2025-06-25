{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
module Charon.Runner.Command
  ( -- * Types
    Command (..),
    CommandP1,
    CommandP2,
    advancePhaseCmd,
    CmdPathF,

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
    RawPathI,
    fromRaw,
  )
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Prelude
import Charon.Runner.Command.List (ListCmd)
import Charon.Runner.Phase
  ( AdvancePhase (advancePhase),
    Phase (Phase1, Phase2),
  )

advancePhaseCmd ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  Command Phase1 ->
  m (Command Phase2)
advancePhaseCmd (Delete paths) = Delete <$> fromRawSet paths
advancePhaseCmd (PermDelete force paths) = PermDelete force <$> fromRawSet paths
advancePhaseCmd (Empty b) = pure (Empty b)
advancePhaseCmd (Restore paths) = Restore <$> fromRawSet paths
advancePhaseCmd Metadata = pure Metadata
advancePhaseCmd (List cfg) = pure $ List $ advancePhase cfg
advancePhaseCmd (Convert dest) = pure $ Convert dest
advancePhaseCmd (Merge dest) = Merge <$> fromRaw dest

fromRawSet ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  UniqueSeqNE (RawPathI i) ->
  m (UniqueSeqNE (PathI i))
fromRawSet =
  fmap USeqNE.unsafeFromFoldable
    . traverse fromRaw
    . USeqNE.toNonEmpty

type CmdPathF :: Phase -> PathIndex -> Type
type family CmdPathF p i where
  CmdPathF Phase1 i = RawPathI i
  CmdPathF Phase2 i = PathI i

-- | Action to run.
type Command :: Phase -> Type
data Command s
  = -- | Deletes a path.
    Delete (UniqueSeqNE (CmdPathF s TrashEntryOriginalPath))
  | -- | Permanently deletes a path from the trash.
    PermDelete
      Bool
      (UniqueSeqNE (CmdPathF s TrashEntryFileName))
  | -- | Empties the trash.
    Empty Bool
  | -- | Restores a path.
    Restore (UniqueSeqNE (CmdPathF s TrashEntryFileName))
  | -- | List all trash contents.
    List (ListCmd s)
  | -- | Prints trash metadata.
    Metadata
  | -- | Converts backend files.
    Convert Backend
  | -- | Merges trash home directories.
    Merge (CmdPathF s TrashHome)

makePrisms ''Command

deriving stock instance Eq (Command Phase1)

deriving stock instance Show (Command Phase1)

deriving stock instance Eq (Command Phase2)

deriving stock instance Show (Command Phase2)

type CommandP1 = Command Phase1

type CommandP2 = Command Phase2
