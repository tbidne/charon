-- | Provides framework for "phased" data. This is a generalization of
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
-- initially parse a data type as Foo1 (phase 1) but then further process
-- this data type into Foo2 (phase 2). Using this phase approach avoids
-- duplication of constructors and fields.
--
-- @since 0.1
module SafeRm.Runner.Phase
  ( Phase (..),
    AdvancePhase (..),
    MaybePhaseF,
  )
where

import SafeRm.Prelude

-- | Index for data that has a "phased" evolution.
--
-- @since 0.1
data Phase
  = Phase1
  | Phase2
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | Advances phased data.
--
-- @since 0.1
class AdvancePhase a where
  -- | The next phase.
  type NextPhase a

  -- | Advances the data.
  --
  -- @since 0.1
  advancePhase :: a -> NextPhase a

-- | Type family for the common case of evolving an optional type @Maybe a@
-- into a definite @a@ (i.e. after default substitution).
--
-- @since 0.1
type MaybePhaseF :: Phase -> Type -> Type
type family MaybePhaseF s a where
  MaybePhaseF Phase1 a = Maybe a
  MaybePhaseF Phase2 a = a
