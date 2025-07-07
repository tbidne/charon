{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for the list command
module Charon.Runner.Command.List
  ( -- * Phase 1
    ListFormatStyle (..),
    parseListFormat,
    ListFormatPhase1 (..),

    -- * Phase 2
    ListCmd (..),
    ListCmdP1,
    ListCmdP2,
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
  ( AdvancePhase (NextPhase, advancePhase),
    MaybePhaseF,
    Phase (Phase1, Phase2),
  )
import Data.Text qualified as T

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
parseListFormat "m" = pure ListFormatStyleMultiline
parseListFormat "tabular" = pure ListFormatStyleTabular
parseListFormat "t" = pure ListFormatStyleTabular
parseListFormat "tabular-simple" = pure ListFormatStyleTabularSimple
parseListFormat "ts" = pure ListFormatStyleTabularSimple
parseListFormat "single" = pure ListFormatStyleSingleline
parseListFormat "s" = pure ListFormatStyleSingleline
parseListFormat other = fail $ "Unrecognized format: " <> T.unpack other

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
type ListFormatPhaseF :: Phase -> Type
type family ListFormatPhaseF s where
  ListFormatPhaseF Phase1 = ListFormatPhase1
  ListFormatPhaseF Phase2 = PathDataFormat

instance AdvancePhase ListFormatPhase1 where
  type NextPhase ListFormatPhase1 = PathDataFormat

  advancePhase formatPhase1 = case formatPhase1 ^. #style of
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
type ListCmd :: Phase -> Type
data ListCmd p = MkListCmd
  { -- | Format style.
    format :: ListFormatPhaseF p,
    -- | How to sort the list.
    sort :: MaybePhaseF p Sort,
    -- | Whether to reverse the sort.
    revSort :: MaybePhaseF p Bool
  }

makeFieldLabelsNoPrefix ''ListCmd

deriving stock instance Eq (ListCmd Phase1)

deriving stock instance Show (ListCmd Phase1)

deriving stock instance Eq (ListCmd Phase2)

deriving stock instance Show (ListCmd Phase2)

type ListCmdP1 = ListCmd Phase1

type ListCmdP2 = ListCmd Phase2

instance AdvancePhase (ListCmd Phase1) where
  type NextPhase (ListCmd Phase1) = ListCmd Phase2

  advancePhase listCfg =
    let sort = case listCfg ^. #sort of
          Just s -> s
          Nothing -> case format of
            FormatSingleline _ -> OriginalPath
            FormatTabularSimple _ -> OriginalPath
            _ -> Name
     in MkListCmd
          { format,
            sort,
            revSort
          }
    where
      revSort = fromMaybe False (listCfg ^. #revSort)
      format = advancePhase (listCfg ^. #format)
