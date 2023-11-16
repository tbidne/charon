{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module SafeRm.Backend.Rosetta
  ( Rosetta (..),
  )
where

import SafeRm.Data.Index (Index)
import SafeRm.Prelude

data Rosetta = MkRosetta
  { index :: Index,
    size :: Bytes B Natural
  }

makeFieldLabelsNoPrefix ''Rosetta
