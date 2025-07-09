{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Runner.Command.PermDelete
  ( PermDeleteParams (..),
    mergePermDelete,
  )
where

import Charon.Prelude
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    IndicesPathsF,
    Prompt (MkPrompt),
    SwitchF,
    Verbose (MkVerbose),
    mergePromptDefTrue,
    parseStrategy,
  )
import Charon.Runner.WithDisabled (fromBool, fromDefault, (<.>?), (<>?))

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
    strategy <- parseStrategy (over' _1 (<> tomlStrategy) argsStrategy)
    pure
      $ MkPermDeleteParams
        { prompt = mergePromptDefTrue $ argsPrompt <>? toml ^. #prompt,
          strategy,
          verbose = argsVerbose <.>? toml ^. #verbose
        }
    where
      tomlStrategy =
        fromBool
          . fromMaybe False
          $ (toml ^. #strategy)
  where
    argsStrategy = args ^. #strategy
    argsPrompt = args ^. #prompt $> MkPrompt True
    argsVerbose = args ^. #verbose $> MkVerbose True
