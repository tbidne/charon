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

    -- ** Sub types
    IndicesPathsStrategy (..),

    -- * Optics
    _Delete,
    _PermDelete,
    _Empty,
    _Restore,
    _List,
    _Metadata,
    _IndicesStrategy,
    _PathsStrategy,
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

-- | Strategy for finding paths in the trash index.
data IndicesPathsStrategy
  = -- | Index strategy i.e. we will show the user the trash with numeric
    -- indices, so that they can specify indices directly.
    IndicesStrategy
  | -- | Normal strategy, explicit trash names are given.
    PathsStrategy (UniqueSeqNE (PathI TrashEntryFileName))
  deriving stock (Eq, Show)

makePrisms ''IndicesPathsStrategy

advancePhaseCmd ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Command Phase1 ->
  m (Command Phase2)
advancePhaseCmd = \case
  Delete paths -> Delete <$> fromRawSet paths
  PermDelete noPrompt s -> PermDelete noPrompt <$> parseStrategy s
  Empty noPrompt -> pure $ Empty noPrompt
  Restore s -> Restore <$> parseStrategy s
  Metadata -> pure Metadata
  List cfg -> pure $ List $ advancePhase cfg
  Convert dest -> pure $ Convert dest
  Merge dest -> Merge <$> fromRaw dest
  where
    parseStrategy = \case
      (True, Nothing) -> pure $ IndicesStrategy
      (True, Just _) -> throwText $ mkStratErr "not both."
      (False, Nothing) -> throwText $ mkStratErr "but none given."
      (False, Just paths) -> PathsStrategy <$> fromRawSet paths

    mkStratErr m =
      mconcat
        [ "Exactly one of --indices or explicit paths required, ",
          m
        ]

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

type IndicesPathsF :: Phase -> Type
type family IndicesPathsF p where
  IndicesPathsF Phase1 = (Bool, (Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))))
  IndicesPathsF Phase2 = IndicesPathsStrategy

-- | Action to run.
type Command :: Phase -> Type
data Command s
  = -- | Deletes a path.
    Delete (UniqueSeqNE (CmdPathF s TrashEntryOriginalPath))
  | -- | Permanently deletes a path from the trash.
    PermDelete
      Bool
      (IndicesPathsF s)
  | -- | Empties the trash.
    Empty Bool
  | -- | Restores a path.
    Restore (IndicesPathsF s)
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
