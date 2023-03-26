{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for the list command
--
-- @since 0.1
module SafeRm.Runner.Command.List
  ( -- * Phase 1
    ListFormatStyle (..),
    parseListFormat,
    ListFormatPhase1 (..),

    -- * Phase 2
    ListCmd (..),
  )
where

import Data.Text qualified as T
import SafeRm.Data.Index (Sort)
import SafeRm.Data.PathData (ColFormat (..), PathDataFormat (..))
import SafeRm.Prelude
import SafeRm.Runner.Phase (AdvancePhase (..), MaybePhaseF, Phase (..))
import SafeRm.Utils qualified as U

--------------------------------------------------------------------------------
----------------------------------- PHASE 1 ------------------------------------
--------------------------------------------------------------------------------

-- | Configuration option for the list command format.
--
-- @since 0.1
data ListFormatStyle
  = -- | @since 0.1
    ListFormatStyleMultiline
  | -- | @since 0.1
    ListFormatStyleTabular
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
parseListFormat :: (MonadFail m) => Text -> m ListFormatStyle
parseListFormat "multi" = pure ListFormatStyleMultiline
parseListFormat "m" = pure ListFormatStyleMultiline
parseListFormat "tabular" = pure ListFormatStyleTabular
parseListFormat "t" = pure ListFormatStyleTabular
parseListFormat other = fail $ "Unrecognized format: " <> T.unpack other

-- | Holds all configuration data for list formatting i.e. style and
-- truncation params.
--
-- @since 0.1
data ListFormatPhase1 = MkListFormatPhase1
  { -- | Format style.
    --
    -- @since 0.1
    style :: !(Maybe ListFormatStyle),
    -- | Name truncation.
    --
    -- @since 0.1
    nameTrunc :: !(Maybe ColFormat),
    -- | Original path truncation.
    --
    -- @since 0.1
    origTrunc :: !(Maybe ColFormat)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''ListFormatPhase1

--------------------------------------------------------------------------------
----------------------------------- PHASE 2 ------------------------------------
--------------------------------------------------------------------------------

-- | Associates the phase to the formatting type.
--
-- @since 0.1
type ListFormatPhaseF :: Phase -> Type
type family ListFormatPhaseF s where
  ListFormatPhaseF Phase1 = ListFormatPhase1
  ListFormatPhaseF Phase2 = PathDataFormat

-- | @since 0.1
instance AdvancePhase ListFormatPhase1 where
  type NextPhase ListFormatPhase1 = PathDataFormat

  advancePhase formatPhase1 = case formatPhase1 ^. #style of
    Just ListFormatStyleMultiline -> FormatMultiline
    Just ListFormatStyleTabular ->
      FormatTabular
        (formatPhase1 ^. #nameTrunc)
        (formatPhase1 ^. #origTrunc)
    Nothing ->
      FormatTabular
        (formatPhase1 ^. #nameTrunc)
        (formatPhase1 ^. #origTrunc)

-- | Arguments for the list command.
--
-- @since 0.1
type ListCmd :: Phase -> Type
data ListCmd p = MkListCmd
  { -- | Format style.
    --
    -- @since 0.1
    format :: !(ListFormatPhaseF p),
    -- | How to sort the list.
    --
    -- @since 0.1
    sort :: !(MaybePhaseF p Sort),
    -- | Whether to reverse the sort.
    --
    -- @since 0.1
    revSort :: !(MaybePhaseF p Bool)
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''ListCmd

-- | @since 0.1
deriving stock instance Eq (ListCmd Phase1)

-- | @since 0.1
deriving stock instance Show (ListCmd Phase1)

-- | @since 0.1
deriving stock instance Eq (ListCmd Phase2)

-- | @since 0.1
deriving stock instance Show (ListCmd Phase2)

-- | @since 0.1
instance AdvancePhase (ListCmd Phase1) where
  type NextPhase (ListCmd Phase1) = ListCmd Phase2

  advancePhase listCfg =
    MkListCmd
      { format,
        sort,
        revSort
      }
    where
      sort = U.fromMaybeMonoid (listCfg ^. #sort)
      revSort = fromMaybe False (listCfg ^. #revSort)
      format = advancePhase (listCfg ^. #format)
