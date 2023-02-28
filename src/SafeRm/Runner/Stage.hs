-- | Provides framework for "staged" data. This is a generalization of
-- the "Higher-kinded data" approach where instead of merely annotating a
-- data type with some constructor e.g.
--
-- @
-- data Foo f = MkFoo
--   { bar :: f Bool,
--     baz :: f String,
--     ...
--   }
-- @
--
-- we use a type family:
--
-- @
-- data Foo s = MkFoo
--   { bar :: Fam1 s,
--     baz :: Fam2 s,
--   ...
--   }
-- @
--
-- Not only does this allow fields to vary independently, it also means
-- we can avoid clunky wrappers like @Identity@.
--
-- We do this so that we can represent data that "evolves" e.g. we may
-- initially parse a data type as Foo1 (stage 1) but then further process
-- this data type into Foo2 (stage 2). Using this staged approach avoids
-- duplication of constructors and fields.
--
-- @since 0.1
module SafeRm.Runner.Stage
  ( Stage (..),
    AdvanceStage (..),
    MaybeStageF,
  )
where

import SafeRm.Prelude

-- | Index for data that has a "staged" evolution.
--
-- @since 0.1
data Stage
  = Stage1
  | Stage2
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Advances staged data.
--
-- @since 0.1
class AdvanceStage a where
  -- | The next stage.
  type NextStage a

  -- | Advances the data.
  --
  -- @since 0.1
  advanceStage :: a -> NextStage a

-- | Type family for the common case of evolving an optional type @Maybe a@
-- into a definite @a@ (i.e. after default substitution).
--
-- @since 0.1
type MaybeStageF :: Stage -> Type -> Type
type family MaybeStageF s a where
  MaybeStageF Stage1 a = Maybe a
  MaybeStageF Stage2 a = a
