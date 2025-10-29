{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Runner.Command.Delete
  ( DeleteParams (..),
    mergeDelete,
  )
where

import Charon.Data.Paths
  ( PathI,
    PathIndex (TrashEntryOriginalPath),
    RawPathI,
  )
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Prelude
import Charon.Runner.Default (fromDefault, (<|.|>))
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged, ConfigPhaseToml),
    Prompt,
    SwitchF,
    Verbose,
    fromRawSet,
    mergePromptDefFalse,
  )

type DeletePathsF :: ConfigPhase -> Type
type family DeletePathsF p where
  DeletePathsF ConfigPhaseArgs = UniqueSeqNE (RawPathI TrashEntryOriginalPath)
  DeletePathsF ConfigPhaseToml = ()
  DeletePathsF ConfigPhaseMerged = UniqueSeqNE (PathI TrashEntryOriginalPath)

data DeleteParams s = MkDeleteParams
  { paths :: DeletePathsF s,
    prompt :: SwitchF s Prompt,
    verbose :: SwitchF s Verbose
  }

makeFieldLabelsNoPrefix ''DeleteParams

deriving stock instance Eq (DeleteParams ConfigPhaseArgs)

deriving stock instance Show (DeleteParams ConfigPhaseArgs)

deriving stock instance Eq (DeleteParams ConfigPhaseToml)

deriving stock instance Show (DeleteParams ConfigPhaseToml)

deriving stock instance Eq (DeleteParams ConfigPhaseMerged)

deriving stock instance Show (DeleteParams ConfigPhaseMerged)

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
          prompt = mergePromptDefFalse argsPrompt,
          verbose = fromDefault argsVerbose
        }
  Just toml -> do
    paths <- fromRawSet argsPaths
    pure
      $ MkDeleteParams
        { paths,
          prompt = mergePromptDefFalse $ argsPrompt <|> toml ^. #prompt,
          verbose = argsVerbose <|.|> toml ^. #verbose
        }
  where
    argsPaths = args ^. #paths
    argsPrompt = args ^. #prompt
    argsVerbose = args ^. #verbose
