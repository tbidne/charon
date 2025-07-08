{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
module Charon.Runner.Command
  ( -- * Types
    Command (..),
    mergeCommand,
    CmdPathF,

    -- ** Delete
    DeleteParams (..),

    -- ** PermDelete
    PermDeleteParams (..),

    -- ** Restore
    RestoreParams (..),

    -- ** Misc types
    Force (..),
    Prompt (..),
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
import Charon.Runner.Command.List (ListCmd, mergeListCommand)
import Charon.Runner.Phase (ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged))
import Charon.Runner.WithDisabled (WithDisabled (Disabled))

newtype Force = MkForce {unForce :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Force

type PromptF :: ConfigPhase -> Type
type family PromptF p where
  PromptF ConfigPhaseArgs = WithDisabled ()
  PromptF ConfigPhaseMerged = Prompt

newtype Prompt = MkPrompt {unPrompt :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Prompt

type CmdPathF :: ConfigPhase -> PathIndex -> Type
type family CmdPathF p i where
  CmdPathF ConfigPhaseArgs i = RawPathI i
  CmdPathF ConfigPhaseMerged i = PathI i

type IndicesPathsF :: ConfigPhase -> Type
type family IndicesPathsF p where
  IndicesPathsF ConfigPhaseArgs = (Bool, (Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))))
  IndicesPathsF ConfigPhaseMerged = IndicesPathsStrategy

-- | Strategy for finding paths in the trash index.
data IndicesPathsStrategy
  = -- | Index strategy i.e. we will show the user the trash with numeric
    -- indices, so that they can specify indices directly.
    IndicesStrategy
  | -- | Normal strategy, explicit trash names are given.
    PathsStrategy (UniqueSeqNE (PathI TrashEntryFileName))
  deriving stock (Eq, Show)

makePrisms ''IndicesPathsStrategy

newtype DeleteParams s = MkDeleteParams
  { paths :: UniqueSeqNE (CmdPathF s TrashEntryOriginalPath)
  }

makeFieldLabelsNoPrefix ''DeleteParams

deriving stock instance Eq (DeleteParams ConfigPhaseArgs)

deriving stock instance Show (DeleteParams ConfigPhaseArgs)

deriving stock instance Eq (DeleteParams ConfigPhaseMerged)

deriving stock instance Show (DeleteParams ConfigPhaseMerged)

data PermDeleteParams s = MkPermDeleteParams
  { prompt :: PromptF s,
    strategy :: IndicesPathsF s
  }

makeFieldLabelsNoPrefix ''PermDeleteParams

deriving stock instance Eq (PermDeleteParams ConfigPhaseArgs)

deriving stock instance Show (PermDeleteParams ConfigPhaseArgs)

deriving stock instance Eq (PermDeleteParams ConfigPhaseMerged)

deriving stock instance Show (PermDeleteParams ConfigPhaseMerged)

data RestoreParams s = MkRestoreParams
  { force :: Force,
    prompt :: PromptF s,
    strategy :: IndicesPathsF s
  }

makeFieldLabelsNoPrefix ''RestoreParams

deriving stock instance Eq (RestoreParams ConfigPhaseArgs)

deriving stock instance Show (RestoreParams ConfigPhaseArgs)

deriving stock instance Eq (RestoreParams ConfigPhaseMerged)

deriving stock instance Show (RestoreParams ConfigPhaseMerged)

-- | Action to run.
type Command :: ConfigPhase -> Type
data Command s
  = -- | Deletes a path.
    Delete (DeleteParams s)
  | -- | Permanently deletes a path from the trash.
    PermDelete (PermDeleteParams s)
  | -- | Empties the trash.
    Empty (PromptF s)
  | -- | Restores a path.
    Restore (RestoreParams s)
  | -- | List all trash contents.
    List (ListCmd s)
  | -- | Prints trash metadata.
    Metadata
  | -- | Converts backend files.
    Convert Backend
  | -- | Merges trash home directories.
    Merge (CmdPathF s TrashHome)

makePrisms ''Command

deriving stock instance Eq (Command ConfigPhaseArgs)

deriving stock instance Show (Command ConfigPhaseArgs)

deriving stock instance Eq (Command ConfigPhaseMerged)

deriving stock instance Show (Command ConfigPhaseMerged)

mergeCommand ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Command ConfigPhaseArgs ->
  m (Command ConfigPhaseMerged)
mergeCommand = \case
  Delete params -> Delete . MkDeleteParams <$> fromRawSet (params ^. #paths)
  PermDelete params -> do
    strategy <- parseStrategy (params ^. #strategy)
    pure
      $ PermDelete
      $ MkPermDeleteParams
        { prompt = mergePromptDefTrue $ params ^. #prompt,
          strategy
        }
  Empty prompt -> pure $ Empty $ mergePromptDefTrue prompt
  Restore params -> do
    strategy <- parseStrategy (params ^. #strategy)
    let params2 =
          MkRestoreParams
            { force = params ^. #force,
              prompt = mergePromptDefTrue $ params ^. #prompt,
              strategy
            }
    pure $ Restore params2
  Metadata -> pure Metadata
  List cfg -> pure $ List $ mergeListCommand cfg
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

    -- For commands that take --prompt, default no answer to True
    -- (i.e. requires prompt).
    mergePromptDefTrue :: WithDisabled () -> Prompt
    mergePromptDefTrue argsPrompt = case argsPrompt of
      Disabled -> MkPrompt False
      _ -> MkPrompt True

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
