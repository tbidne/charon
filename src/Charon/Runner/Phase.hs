{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Runner.Phase
  ( -- * Types
    Force (..),
    IndicesPathsStrategy (..),
    Prompt (..),
    Verbose (..),

    -- * Phase
    ConfigPhase (..),

    -- * Type families
    ConfigPhaseF,
    IndicesPathsF,
    SwitchF,

    -- * Misc
    fromRawSet,
    mergePromptDefTrue,
    parseStrategy,

    -- * Optics
    _IndicesStrategy,
    _PathsStrategy,
  )
where

import Charon.Data.Paths
  ( PathI,
    PathIndex (TrashEntryFileName),
    RawPathI,
    fromRaw,
  )
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Prelude
import Charon.Runner.Default (Default (def))
import Charon.Runner.WithDisabled (WithDisabled (Disabled, With, Without))

newtype Force = MkForce {unForce :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Force

instance Default Force where
  def = MkForce False

newtype Prompt = MkPrompt {unPrompt :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Prompt

newtype Verbose = MkVerbose {unVerbose :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Verbose

instance Default Verbose where
  def = MkVerbose False

data ConfigPhase
  = ConfigPhaseArgs
  | ConfigPhaseToml
  | ConfigPhaseMerged
  | ConfigPhaseEnv

type ConfigPhaseF :: ConfigPhase -> Type -> Type
type family ConfigPhaseF p a where
  ConfigPhaseF ConfigPhaseArgs a = Maybe a
  ConfigPhaseF ConfigPhaseToml a = Maybe a
  ConfigPhaseF ConfigPhaseMerged a = a
  ConfigPhaseF ConfigPhaseEnv a = a

type SwitchF :: ConfigPhase -> Type -> Type
type family SwitchF p a where
  SwitchF ConfigPhaseArgs _ = WithDisabled ()
  SwitchF ConfigPhaseToml a = Maybe a
  SwitchF ConfigPhaseMerged a = a

type IndicesPathsF :: ConfigPhase -> Type
type family IndicesPathsF p where
  IndicesPathsF ConfigPhaseArgs = (WithDisabled (), (Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))))
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

parseStrategy ::
  ( HasCallStack,
    MonadCatch m,
    MonadPathReader m,
    MonadTerminal m
  ) =>
  (WithDisabled (), Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))) ->
  m IndicesPathsStrategy
parseStrategy = \case
  (With _, Nothing) -> pure $ IndicesStrategy
  (With _, Just _) -> throwText $ mkStratErr "not both."
  (Without, Nothing) -> throwText $ mkStratErr "but none given."
  (Without, Just paths) -> PathsStrategy <$> fromRawSet paths
  (Disabled, Nothing) -> throwText $ mkStratErr "but none given."
  (Disabled, Just paths) -> PathsStrategy <$> fromRawSet paths
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
