{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides functionality for the list command
--
-- @since 0.1
module SafeRm.Runner.Command.List
  ( -- * Stage 1
    ListFormatStyle (..),
    parseListFormat,
    ListFormatStage1 (..),

    -- * Stage 2
    ListCmd (..),
  )
where

import Data.Text qualified as T
import SafeRm.Data.Index (Sort)
import SafeRm.Data.PathData (PathDataFormat (..))
import SafeRm.Prelude
import SafeRm.Runner.Stage (AdvanceStage (..), MaybeStageF, Stage (..))
import SafeRm.Utils qualified as U

--------------------------------------------------------------------------------
----------------------------------- STAGE 1 ------------------------------------
--------------------------------------------------------------------------------

-- | Configuration option for the list command format.
--
-- @since 0.1
data ListFormatStyle
  = -- | @since 0.1
    ListFormatStyleMultiline
  | -- | @since 0.1
    ListFormatStyleTabular
  | -- | @since 0.1
    ListFormatStyleAuto
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
parseListFormat "auto" = pure ListFormatStyleAuto
parseListFormat "a" = pure ListFormatStyleAuto
parseListFormat other = fail $ "Unrecognized format: " <> T.unpack other

-- | Holds all configuration data for list formatting i.e. style and
-- truncation params.
--
-- @since 0.1
data ListFormatStage1 = MkListFormatStage1
  { -- | Format style.
    --
    -- @since 0.1
    style :: !(Maybe ListFormatStyle),
    -- | Name truncation.
    --
    -- @since 0.1
    nameTrunc :: !(Maybe Natural),
    -- | Original path truncation.
    --
    -- @since 0.1
    origTrunc :: !(Maybe Natural)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''ListFormatStage1

--------------------------------------------------------------------------------
----------------------------------- STAGE 2 ------------------------------------
--------------------------------------------------------------------------------

-- | Associates the stage to the formatting type.
--
-- @since 0.1
type ListFormatStageF :: Stage -> Type
type family ListFormatStageF s where
  ListFormatStageF Stage1 = ListFormatStage1
  ListFormatStageF Stage2 = PathDataFormat

-- | @since 0.1
instance AdvanceStage ListFormatStage1 where
  type NextStage ListFormatStage1 = PathDataFormat

  advanceStage formatStage1 = case formatStage1 ^. #style of
    Just ListFormatStyleMultiline -> FormatMultiline
    Just ListFormatStyleTabular ->
      FormatTabular
        (fromMaybe 10 (formatStage1 ^. #nameTrunc))
        (fromMaybe 22 (formatStage1 ^. #origTrunc))
    Just ListFormatStyleAuto -> FormatTabularAuto
    -- default to FormatTabularAuto
    Nothing -> FormatTabularAuto

-- | Arguments for the list command.
--
-- @since 0.1
type ListCmd :: Stage -> Type
data ListCmd s = MkListCmd
  { -- | Format style.
    --
    -- @since 0.1
    format :: !(ListFormatStageF s),
    -- | How to sort the list.
    --
    -- @since 0.1
    sort :: !(MaybeStageF s Sort),
    -- | Whether to reverse the sort.
    --
    -- @since 0.1
    revSort :: !(MaybeStageF s Bool)
  }

-- | @since 0.1
makeFieldLabelsNoPrefix ''ListCmd

-- | @since 0.1
deriving stock instance Eq (ListCmd Stage1)

-- | @since 0.1
deriving stock instance Show (ListCmd Stage1)

-- | @since 0.1
deriving stock instance Eq (ListCmd Stage2)

-- | @since 0.1
deriving stock instance Show (ListCmd Stage2)

-- | @since 0.1
instance AdvanceStage (ListCmd Stage1) where
  type NextStage (ListCmd Stage1) = ListCmd Stage2

  advanceStage listCfg =
    MkListCmd
      { format,
        sort,
        revSort
      }
    where
      sort = U.fromMaybeMonoid (listCfg ^. #sort)
      revSort = fromMaybe False (listCfg ^. #revSort)
      format = advanceStage (listCfg ^. #format)
