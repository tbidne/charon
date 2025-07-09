{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Runner.Command.Restore
  ( RestoreParams (..),
    mergeRestore,
  )
where

import Charon.Prelude
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    Force (MkForce),
    IndicesPathsF,
    Prompt (MkPrompt),
    SwitchF,
    Verbose (MkVerbose),
    mergePromptDefTrue,
    parseStrategy,
  )
import Charon.Runner.WithDisabled (fromBool, fromDefault, (<.>?), (<>?))

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
    strategy <- parseStrategy (over' _1 (<> tomlStrategy) argsStrategy)
    pure
      $ MkRestoreParams
        { force = argsForce <.>? toml ^. #force,
          prompt = mergePromptDefTrue $ argsPrompt <>? toml ^. #prompt,
          strategy,
          verbose = argsVerbose <.>? toml ^. #verbose
        }
    where
      tomlStrategy =
        fromBool
          . fromMaybe False
          $ (toml ^. #strategy)
  where
    argsForce = args ^. #force $> MkForce True
    argsStrategy = args ^. #strategy
    argsPrompt = args ^. #prompt $> MkPrompt True
    argsVerbose = args ^. #verbose $> MkVerbose True
