{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for the list command
module Charon.Runner.Command.List
  ( -- * Phase 1
    ListFormatStyle (..),
    parseListFormat,
    ListFormatPhase1 (..),

    -- * Phase 2
    ListParams (..),
    mergeList,
  )
where

import Charon.Data.PathData.Formatting
  ( ColFormat,
    Coloring (ColoringDetect),
    PathDataFormat (FormatMultiline, FormatSingleline, FormatTabular, FormatTabularSimple),
    Sort (Name, OriginalPath),
  )
import Charon.Prelude
import Charon.Runner.Phase
  ( ConfigPhase (ConfigPhaseArgs, ConfigPhaseMerged),
    ConfigPhaseF,
  )

--------------------------------------------------------------------------------
----------------------------------- PHASE 1 ------------------------------------
--------------------------------------------------------------------------------

-- | Configuration option for the list command format.
data ListFormatStyle
  = ListFormatStyleMultiline
  | ListFormatStyleTabular
  | ListFormatStyleTabularSimple
  | ListFormatStyleSingleline
  deriving stock (Eq, Show)

parseListFormat :: (MonadFail m) => Text -> m ListFormatStyle
parseListFormat "multi" = pure ListFormatStyleMultiline
parseListFormat "tabular" = pure ListFormatStyleTabular
parseListFormat "tabular-simple" = pure ListFormatStyleTabularSimple
parseListFormat "single" = pure ListFormatStyleSingleline
parseListFormat other = fail $ "Unrecognized format: " <> unpackText other

-- | Holds all configuration data for list formatting i.e. style and
-- truncation params.
data ListFormatPhase1 = MkListFormatPhase1
  { coloring :: Maybe Coloring,
    -- | Format style.
    style :: Maybe ListFormatStyle,
    -- | Name truncation.
    nameTrunc :: Maybe ColFormat,
    -- | Original path truncation.
    origTrunc :: Maybe ColFormat
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''ListFormatPhase1

--------------------------------------------------------------------------------
----------------------------------- PHASE 2 ------------------------------------
--------------------------------------------------------------------------------

-- | Associates the phase to the formatting type.
type ListFormatPhaseF :: ConfigPhase -> Type
type family ListFormatPhaseF s where
  ListFormatPhaseF ConfigPhaseArgs = ListFormatPhase1
  ListFormatPhaseF ConfigPhaseMerged = PathDataFormat

mergeFormat :: ListFormatPhase1 -> PathDataFormat
mergeFormat formatPhase1 = case formatPhase1 ^. #style of
  Just ListFormatStyleMultiline -> FormatMultiline
  Just ListFormatStyleSingleline -> FormatSingleline coloring
  Just ListFormatStyleTabular ->
    FormatTabular
      coloring
      (formatPhase1 ^. #nameTrunc)
      (formatPhase1 ^. #origTrunc)
  Just ListFormatStyleTabularSimple -> FormatTabularSimple coloring
  Nothing ->
    FormatTabular
      coloring
      (formatPhase1 ^. #nameTrunc)
      (formatPhase1 ^. #origTrunc)
  where
    coloring = fromMaybe ColoringDetect (formatPhase1 ^. #coloring)

-- | Arguments for the list command.
type ListParams :: ConfigPhase -> Type
data ListParams p = MkListParams
  { -- | Format style.
    format :: ListFormatPhaseF p,
    -- | How to sort the list.
    sort :: ConfigPhaseF p Sort,
    -- | Whether to reverse the sort.
    revSort :: ConfigPhaseF p Bool
  }

makeFieldLabelsNoPrefix ''ListParams

deriving stock instance Eq (ListParams ConfigPhaseArgs)

deriving stock instance Show (ListParams ConfigPhaseArgs)

deriving stock instance Eq (ListParams ConfigPhaseMerged)

deriving stock instance Show (ListParams ConfigPhaseMerged)

mergeList :: ListParams ConfigPhaseArgs -> ListParams ConfigPhaseMerged
mergeList listCfg =
  let sort = case listCfg ^. #sort of
        Just s -> s
        Nothing -> case format of
          FormatSingleline _ -> OriginalPath
          FormatTabularSimple _ -> OriginalPath
          _ -> Name
   in MkListParams
        { format,
          sort,
          revSort
        }
  where
    revSort = fromMaybe False (listCfg ^. #revSort)
    format = mergeFormat (listCfg ^. #format)
