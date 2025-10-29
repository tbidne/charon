{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Runner.Command.Restore
  ( RestoreParams (..),
    mergeRestore,
  )
where

import Charon.Prelude
import Charon.Runner.Default (fromDefault, (<|.|>))
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    Force,
    IndicesPathsF,
    Prompt,
    SwitchF,
    Verbose,
    mergePromptDefTrue,
    parseStrategy,
  )

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
    strategy <- parseStrategy (over' _1 (<|> tomlStrategy) argsStrategy)
    pure
      $ MkRestoreParams
        { force = argsForce <|.|> toml ^. #force,
          prompt = mergePromptDefTrue $ argsPrompt <|> toml ^. #prompt,
          strategy,
          verbose = argsVerbose <|.|> toml ^. #verbose
        }
    where
      tomlStrategy = toml ^. #strategy
  where
    argsForce = args ^. #force
    argsStrategy = args ^. #strategy
    argsPrompt = args ^. #prompt
    argsVerbose = args ^. #verbose
