{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Charon.Backend.Rosetta
  ( Rosetta (..),
  )
where

import Charon.Data.Index (Index)
import Charon.Prelude

data Rosetta = MkRosetta
  { index :: Index,
    size :: Bytes B Natural
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Rosetta
