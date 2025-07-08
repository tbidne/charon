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
    Verbose (..),

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
import Charon.Runner.Default (Default (def))
import Charon.Runner.Phase
  ( ConfigPhase
      ( ConfigPhaseArgs,
        ConfigPhaseMerged,
        ConfigPhaseToml
      ),
  )
import Charon.Runner.WithDisabled
  ( WithDisabled (Disabled, With, Without),
    fromDefault,
    (<.>?),
    (<>?),
  )

newtype Force = MkForce {unForce :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Force

instance Default Force where
  def = MkForce False

type SwitchF :: ConfigPhase -> Type -> Type
type family SwitchF p a where
  SwitchF ConfigPhaseArgs _ = WithDisabled ()
  SwitchF ConfigPhaseToml a = Maybe a
  SwitchF ConfigPhaseMerged a = a

newtype Prompt = MkPrompt {unPrompt :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Prompt

newtype Verbose = MkVerbose {unVerbose :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Verbose

instance Default Verbose where
  def = MkVerbose False

type CmdPathF :: ConfigPhase -> PathIndex -> Type
type family CmdPathF p i where
  CmdPathF ConfigPhaseArgs i = RawPathI i
  CmdPathF ConfigPhaseMerged i = PathI i

type IndicesPathsF :: ConfigPhase -> Type
type family IndicesPathsF p where
  IndicesPathsF ConfigPhaseArgs = (Bool, (Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))))
  IndicesPathsF ConfigPhaseToml = Maybe Bool
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

type DeletePathsF :: ConfigPhase -> Type
type family DeletePathsF p where
  DeletePathsF ConfigPhaseArgs = UniqueSeqNE (RawPathI TrashEntryOriginalPath)
  DeletePathsF ConfigPhaseToml = ()
  DeletePathsF ConfigPhaseMerged = UniqueSeqNE (PathI TrashEntryOriginalPath)

data DeleteParams s = MkDeleteParams
  { paths :: DeletePathsF s,
    verbose :: SwitchF s Verbose
  }

makeFieldLabelsNoPrefix ''DeleteParams

deriving stock instance Eq (DeleteParams ConfigPhaseArgs)

deriving stock instance Show (DeleteParams ConfigPhaseArgs)

deriving stock instance Eq (DeleteParams ConfigPhaseToml)

deriving stock instance Show (DeleteParams ConfigPhaseToml)

deriving stock instance Eq (DeleteParams ConfigPhaseMerged)

deriving stock instance Show (DeleteParams ConfigPhaseMerged)

data PermDeleteParams s = MkPermDeleteParams
  { prompt :: SwitchF s Prompt,
    strategy :: IndicesPathsF s,
    verbose :: SwitchF s Verbose
  }

makeFieldLabelsNoPrefix ''PermDeleteParams

deriving stock instance Eq (PermDeleteParams ConfigPhaseArgs)

deriving stock instance Show (PermDeleteParams ConfigPhaseArgs)

deriving stock instance Eq (PermDeleteParams ConfigPhaseToml)

deriving stock instance Show (PermDeleteParams ConfigPhaseToml)

deriving stock instance Eq (PermDeleteParams ConfigPhaseMerged)

deriving stock instance Show (PermDeleteParams ConfigPhaseMerged)

data RestoreParams s = MkRestoreParams
  { force :: SwitchF s Force,
    prompt :: SwitchF s Prompt,
    strategy :: IndicesPathsF s,
    verbose :: SwitchF s Verbose
  }

makeFieldLabelsNoPrefix ''RestoreParams

deriving stock instance Eq (RestoreParams ConfigPhaseArgs)

deriving stock instance Show (RestoreParams ConfigPhaseArgs)

deriving stock instance Eq (RestoreParams ConfigPhaseToml)

deriving stock instance Show (RestoreParams ConfigPhaseToml)

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
    Empty (SwitchF s Prompt)
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

-- Need Toml config but cyclic dep!!!
--
-- Can we pass in the Config separately?
-- Annoying, but yes.

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
  Empty prompt -> do
    let merged = prompt $> MkPrompt True
    pure $ Empty $ mergePromptDefTrue merged
  Restore params -> do
    merged <- mergeRestore params restoreParams
    pure $ Restore merged
  Metadata -> pure Metadata
  List cfg -> pure $ List $ mergeListCommand cfg
  Convert dest -> pure $ Convert dest
  Merge dest -> Merge <$> fromRaw dest

mergeDelete ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  DeleteParams ConfigPhaseArgs ->
  Maybe (DeleteParams ConfigPhaseToml) ->
  m (DeleteParams ConfigPhaseMerged)
mergeDelete args = \case
  Nothing -> do
    paths <- fromRawSet argsPaths
    pure
      $ MkDeleteParams
        { paths,
          verbose = fromDefault argsVerbose
        }
  Just toml -> do
    paths <- fromRawSet argsPaths
    pure
      $ MkDeleteParams
        { paths,
          verbose = argsVerbose <.>? toml ^. #verbose
        }
  where
    argsPaths = args ^. #paths
    argsVerbose = args ^. #verbose $> MkVerbose True

-- Basic idea for how args and toml switches are combined.
--
-- - On the CLI, '--foo' is true and '--no-foo' is false.
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

mergePermDelete ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  PermDeleteParams ConfigPhaseArgs ->
  Maybe (PermDeleteParams ConfigPhaseToml) ->
  m (PermDeleteParams ConfigPhaseMerged)
mergePermDelete args = \case
  Nothing -> do
    strategy <- parseStrategy argsStrategy
    pure
      $ MkPermDeleteParams
        { prompt = mergePromptDefTrue argsPrompt,
          strategy,
          verbose = fromDefault argsVerbose
        }
  Just toml -> do
    strategy <- parseStrategy (over' _1 (|| tomlStrategy) argsStrategy)
    pure
      $ MkPermDeleteParams
        { prompt = mergePromptDefTrue $ argsPrompt <>? toml ^. #prompt,
          strategy,
          verbose = argsVerbose <.>? toml ^. #verbose
        }
    where
      tomlStrategy = fromMaybe False (toml ^. #strategy)
  where
    argsStrategy = args ^. #strategy
    argsPrompt = args ^. #prompt $> MkPrompt True
    argsVerbose = args ^. #verbose $> MkVerbose True

mergeRestore ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  RestoreParams ConfigPhaseArgs ->
  Maybe (RestoreParams ConfigPhaseToml) ->
  m (RestoreParams ConfigPhaseMerged)
mergeRestore args = \case
  Nothing -> do
    strategy <- parseStrategy argsStrategy
    pure
      $ MkRestoreParams
        { force = fromDefault argsForce,
          prompt = mergePromptDefTrue argsPrompt,
          strategy,
          verbose = fromDefault argsVerbose
        }
  Just toml -> do
    strategy <- parseStrategy (over' _1 (|| tomlStrategy) argsStrategy)
    pure
      $ MkRestoreParams
        { force = argsForce <.>? toml ^. #force,
          prompt = mergePromptDefTrue $ argsPrompt <>? toml ^. #prompt,
          strategy,
          verbose = argsVerbose <.>? toml ^. #verbose
        }
    where
      tomlStrategy = fromMaybe False (toml ^. #strategy)
  where
    argsForce = args ^. #force $> MkForce True
    argsStrategy = args ^. #strategy
    argsPrompt = args ^. #prompt $> MkPrompt True
    argsVerbose = args ^. #verbose $> MkVerbose True

parseStrategy ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  (Bool, Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))) ->
  m IndicesPathsStrategy
parseStrategy = \case
  (True, Nothing) -> pure $ IndicesStrategy
  (True, Just _) -> throwText $ mkStratErr "not both."
  (False, Nothing) -> throwText $ mkStratErr "but none given."
  (False, Just paths) -> PathsStrategy <$> fromRawSet paths
  where
    mkStratErr m =
      mconcat
        [ "Exactly one of --indices or explicit paths required, ",
          m
        ]

-- For commands that take --prompt, default no answer to True
-- (i.e. requires prompt).
--
-- Note that we cannot use the usual 'x <.>? y' to invoke a hypothetical
-- Default instance for Prompt (True), because it will be this default
-- (True) for Disabled, but we want False.
mergePromptDefTrue :: WithDisabled Prompt -> Prompt
mergePromptDefTrue argsPrompt = case argsPrompt of
  -- 1. If Prompt is actually specified we should use it.
  With p -> p
  -- 2. This is the 'def true' part. If it is not specified,
  --    it should be true.
  Without -> MkPrompt True
  -- 3. Disabled is always false.
  Disabled -> MkPrompt False

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
