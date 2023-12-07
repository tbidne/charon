{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Backend.Rosetta
  ( Rosetta (..),
  )
where

import Charon.Data.Index (Index)
import Charon.Prelude

-- | Rosetta is used to convert from/to backend. If we want to covert backends
-- b -> c, we essentially do c.fromRosetta (b.toRosetta()).
data Rosetta = MkRosetta
  { index :: Index,
    size :: Bytes B Natural
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Rosetta
