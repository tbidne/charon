module Utils
  ( assertSameOrder,
  )
where

import Data.HashSet qualified as HSet
import Hedgehog (PropertyT)
import Unit.Prelude

-- | Asserts that the UniqueSeq matches the expected order.
assertSameOrder ::
  -- | Expected order, duplicates allowed
  [Int] ->
  -- | Actual
  [Int] ->
  PropertyT IO ()
assertSameOrder = go HSet.empty
  where
    go :: HashSet Int -> [Int] -> [Int] -> PropertyT IO ()
    -- Base case 1: both lists are empty -> equal
    go _ [] [] = pure ()
    -- Base case 2: actual is non-empty but expected is empty -> failure
    go _ [] actual@(_ : _) = do
      annotate "Actual has more elements than expected"
      annotateShow actual
      failure
    -- Base case 3: actual is empty but expected is not -> okay as long
    -- as every element left is a duplicate.
    go found expected' [] =
      for_ expected' $ \i -> do
        annotateShow expected'
        annotateShow found
        annotateShow i
        assert (HSet.member i found)
    -- Inductive case
    go found (e : es) (a : as)
      -- a and e are equal -> okay
      | e == a = go found' es as
      -- Not equal -> e _should_ be a duplicate, so verify and skip all
      -- other dupes before continuing.
      | otherwise = do
          if HSet.member e found
            then -- Expected value e is a duplicate -> okay. Try again on the
            -- rest of the expected list.
              go found es (a : as)
            else -- Original value e is not a duplicate -> failure.
            do
              annotate "Non-duplicate missing from new list"
              annotateShow e
              failure
      where
        found' = HSet.insert a found
