module Integration.AsciiOnly
  ( AsciiOnly (..),
  )
where

import Data.Tagged (Tagged (..))
import SafeRm.Prelude
import Test.Tasty.Options (IsOption (..), flagCLParser, safeReadBool)

-- | Option for limiting hedgehog-generated strings to ascii, for easier
-- debugging.
newtype AsciiOnly = MkAsciiOnly Bool
  deriving stock (Eq, Show)

instance IsOption AsciiOnly where
  defaultValue = MkAsciiOnly False
  parseValue = fmap MkAsciiOnly . safeReadBool
  optionName = Tagged "ascii-only"
  optionHelp = Tagged "Limits the generated strings to ascii."
  optionCLParser = flagCLParser Nothing (MkAsciiOnly True)
