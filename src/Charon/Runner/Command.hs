{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Command' type.
module Charon.Runner.Command
  ( -- * Types
    Command (..),
    mergeCommand,
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
      ( TrashHome
      ),
    RawPathI,
    fromRaw,
  )
import Charon.Prelude
import Charon.Runner.Command.Delete (DeleteParams, mergeDelete)
import Charon.Runner.Command.List (ListParams, mergeList)
import Charon.Runner.Command.PermDelete (PermDeleteParams, mergePermDelete)
import Charon.Runner.Command.Restore (RestoreParams, mergeRestore)
import Charon.Runner.Phase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
    Prompt,
    SwitchF,
    mergePromptDefTrue,
  )

type CmdPathF :: ConfigPhase -> PathIndex -> Type
type family CmdPathF p i where
  CmdPathF ConfigPhaseArgs i = RawPathI i
  CmdPathF ConfigPhaseMerged i = PathI i

-- | Action to run.
type Command :: ConfigPhase -> Type
data Command s
  = -- | Deletes a path.
    Delete (DeleteParams s)
  | -- | Permanently deletes a path from the trash.
    PermDelete (PermDeleteParams s)
  | -- | Empties the trash.
    Empty (SwitchF s Prompt)
  | -- | Restores a path.
    Restore (RestoreParams s)
  | -- | List all trash contents.
    List (ListParams s)
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

-- Basic idea for how args and toml switches are combined.
--
-- - On the CLI, '--foo on' is true and '--foo off' is false.
-- - On the TOML, 'foo = true' is true and 'foo = false' is false.
-- - CLI and TOML are merged s.t. if the CLI is specified _at all_, it
--   overrides the TOML
-- - If nothing is specified, default behavior takes over.
--
-- E.g. --verbose defaults to false, so if neither CLI nor TOML specify it,
-- it is false.
--
-- OTOH, --prompt defaults to true, so if neither CLI nor TOML specify it,
-- it is true.

mergeCommand ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m,
    MonadThrow m
  ) =>
  Maybe (DeleteParams ConfigPhaseToml) ->
  Maybe (PermDeleteParams ConfigPhaseToml) ->
  Maybe (RestoreParams ConfigPhaseToml) ->
  Command ConfigPhaseArgs ->
  m (Command ConfigPhaseMerged)
mergeCommand deleteParams permDeleteParams restoreParams = \case
  Delete params -> do
    merged <- mergeDelete params deleteParams
    pure $ Delete merged
  PermDelete params -> do
    merged <- mergePermDelete params permDeleteParams
    pure $ PermDelete merged
  Empty prompt -> pure $ Empty $ mergePromptDefTrue prompt
  Restore params -> do
    merged <- mergeRestore params restoreParams
    pure $ Restore merged
  Metadata -> pure Metadata
  List cfg -> pure $ List $ mergeList cfg
  Convert dest -> pure $ Convert dest
  Merge dest -> Merge <$> fromRaw dest
