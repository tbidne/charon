module Charon.Runner.Default
  ( Default (..),
    fromDefault,
    (<|.|>),
  )
where

import Charon.Prelude

-- | For types with a default value.
class Default a where
  def :: a

instance Default (Maybe a) where
  def = Nothing

fromDefault :: (Default a) => Maybe a -> a
fromDefault = fromMaybe def

(<|.|>) :: (Default a) => Maybe a -> Maybe a -> a
x <|.|> y = fromDefault (x <|> y)

infixr 6 <|.|>
