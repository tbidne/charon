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
    mergePromptDefFalse,
    parseStrategy,
    parseSwitch,

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
import TOML (DecodeTOML (tomlDecoder))

parseSwitch :: (MonadFail m) => Text -> m Bool
parseSwitch = \case
  "on" -> pure True
  "off" -> pure False
  other ->
    fail
      $ mconcat
        [ "Expected (on | off), received: '",
          unpackText other,
          "'"
        ]

newtype Force = MkForce {unForce :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Force

instance Default Force where
  def = MkForce False

instance DecodeTOML Force where
  tomlDecoder = MkForce <$> (tomlDecoder >>= parseSwitch)

-- Note no Default instance since we not all commands have the same prompt
-- behavior.
newtype Prompt = MkPrompt {unPrompt :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Prompt

instance DecodeTOML Prompt where
  tomlDecoder = MkPrompt <$> (tomlDecoder >>= parseSwitch)

newtype Verbose = MkVerbose {unVerbose :: Bool}
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Verbose

instance Default Verbose where
  def = MkVerbose False

instance DecodeTOML Verbose where
  tomlDecoder = MkVerbose <$> (tomlDecoder >>= parseSwitch)

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
  SwitchF ConfigPhaseArgs a = Maybe a
  SwitchF ConfigPhaseToml a = Maybe a
  SwitchF ConfigPhaseMerged a = a

type IndicesPathsF :: ConfigPhase -> Type
type family IndicesPathsF p where
  IndicesPathsF ConfigPhaseArgs = (Maybe Bool, Maybe (UniqueSeqNE (RawPathI TrashEntryFileName)))
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
  (Maybe Bool, Maybe (UniqueSeqNE (RawPathI TrashEntryFileName))) ->
  m IndicesPathsStrategy
parseStrategy = \case
  -- Indices strategy explicitly on
  (Just True, Nothing) -> pure IndicesStrategy
  (Just True, Just _) -> throwText $ mkStratErr "not both."
  -- Indices strategy explicitly off
  (Just False, Nothing) -> throwText $ mkStratErr "but none given."
  (Just False, Just paths) -> PathsStrategy <$> fromRawSet paths
  -- Indices strategy not specified
  (Nothing, Nothing) -> throwText $ mkStratErr "but none given."
  (Nothing, Just paths) -> PathsStrategy <$> fromRawSet paths
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
mergePromptDefTrue :: Maybe Prompt -> Prompt
mergePromptDefTrue argsPrompt = case argsPrompt of
  -- 1. If Prompt is actually specified we should use it.
  Just p -> p
  -- 2. This is the 'def true' part. If it is not specified,
  --    it should be true.
  Nothing -> MkPrompt True

mergePromptDefFalse :: Maybe Prompt -> Prompt
mergePromptDefFalse argsPrompt = case argsPrompt of
  -- 1. If Prompt is actually specified we should use it.
  Just p -> p
  -- 2. This is the 'def false' part. If it is not specified,
  --    it should be false.
  Nothing -> MkPrompt False
