{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The module contains configuration shared between CLI Args and TOML.
--
-- @since 0.1
module SafeRm.Runner.Config
  ( CmdListCfg (..),
    ListFormatCfg (..),
    parseListFormat,
  )
where

import Data.Text qualified as T
import SafeRm.Data.Index (Sort, readSort)
import SafeRm.Prelude
import TOML (DecodeTOML (..), getFieldOpt, getFieldOptWith)

-- | @since 0.1
data ListFormatCfg
  = -- | @since 0.1
    FormatMultilineCfg
  | -- | @since 0.1
    FormatTabularCfg
  | -- | @since 0.1
    FormatAutoCfg
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance DecodeTOML ListFormatCfg where
  tomlDecoder = tomlDecoder >>= parseListFormat

-- | @since 0.1
parseListFormat :: (MonadFail m) => Text -> m ListFormatCfg
parseListFormat "multi" = pure FormatMultilineCfg
parseListFormat "m" = pure FormatMultilineCfg
parseListFormat "tabular" = pure FormatTabularCfg
parseListFormat "t" = pure FormatTabularCfg
parseListFormat "auto" = pure FormatAutoCfg
parseListFormat "a" = pure FormatAutoCfg
parseListFormat other = fail $ "Unrecognized format: " <> T.unpack other

-- | Configuration for the list command.
--
-- @since 0.1
data CmdListCfg = MkCmdListCfg
  { -- | Whether to print the list in multiline format i.e. each index
    -- entry gets its own line.
    --
    -- @since 0.1
    format :: !(Maybe ListFormatCfg),
    -- | Truncates the name length.
    --
    -- @since 0.1
    nameTrunc :: !(Maybe Natural),
    -- | Truncates the original path length.
    --
    -- @since 0.1
    origTrunc :: !(Maybe Natural),
    -- | How to sort the list.
    --
    -- @since 0.1
    sort :: !(Maybe Sort),
    -- | Whether to reverse the sort.
    --
    -- @since 0.1
    revSort :: !(Maybe Bool)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
makeFieldLabelsNoPrefix ''CmdListCfg

-- | @since 0.1
instance Semigroup CmdListCfg where
  MkCmdListCfg a b c d e <> MkCmdListCfg a' b' c' d' e' =
    MkCmdListCfg (a <|> a') (b <|> b') (c <|> c') (d <|> d') (e <|> e')

-- | @since 0.1
instance Monoid CmdListCfg where
  mempty = MkCmdListCfg Nothing Nothing Nothing Nothing Nothing

-- | @since 0.1
instance DecodeTOML CmdListCfg where
  tomlDecoder =
    MkCmdListCfg
      <$> getFieldOpt "format"
      <*> getFieldOpt "name-trunc"
      <*> getFieldOpt "orig-trunc"
      <*> getFieldOptWith (tomlDecoder >>= readSort) "sort"
      <*> getFieldOpt "reverse-sort"
