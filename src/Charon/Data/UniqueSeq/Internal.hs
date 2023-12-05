{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeq' type.
module Charon.Data.UniqueSeq.Internal
  ( -- * Type
    UniqueSeq (MkUniqueSeq, UnsafeUniqueSeq),

    -- * Creation
    fromFoldable,

    -- * Operations
    prepend,
    append,
    union,

    -- * Utils
    insertSeq,
    notHSetMember,
  )
where

import Charon.Prelude
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import GHC.IsList (IsList (Item, fromList, toList))

-- | Like 'Seq' but with the guarantee that all elements are unique. This
-- comes with the cost of O(2n) space.
data UniqueSeq a = UnsafeUniqueSeq
  { seq :: Seq a,
    set :: HashSet a
  }
  deriving stock (Eq, Show)

instance Foldable UniqueSeq where
  foldr f x (UnsafeUniqueSeq seq _) = foldr f x seq

pattern MkUniqueSeq :: Seq a -> HashSet a -> UniqueSeq a
pattern MkUniqueSeq seq set <- UnsafeUniqueSeq seq set

{-# COMPLETE MkUniqueSeq #-}

instance
  (k ~ A_Getter, b ~ Seq a, c ~ Seq a) =>
  LabelOptic "seq" k (UniqueSeq a) (UniqueSeq a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeq seq _) -> seq)

instance
  (k ~ A_Getter, b ~ HashSet a, c ~ HashSet a) =>
  LabelOptic "set" k (UniqueSeq a) (UniqueSeq a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeq _ set) -> set)

instance (Hashable a) => Semigroup (UniqueSeq a) where
  (<>) = union

instance (Hashable a) => Monoid (UniqueSeq a) where
  mempty = UnsafeUniqueSeq Seq.empty HSet.empty

instance (Hashable a) => IsList (UniqueSeq a) where
  type Item (UniqueSeq a) = a
  toList (UnsafeUniqueSeq seq _) = toList seq
  fromList = fromFoldable

union :: forall a. (Hashable a) => UniqueSeq a -> UniqueSeq a -> UniqueSeq a
union (UnsafeUniqueSeq xseq _) (UnsafeUniqueSeq yseq _) =
  UnsafeUniqueSeq newSeq newSet
  where
    -- To preserve order, we must fold from the left
    (newSeq, newSet) = foldl' go (Seq.empty, HSet.empty) (xseq <> yseq)
    go :: (Seq a, HashSet a) -> a -> (Seq a, HashSet a)
    go (accSeq, accSet) z
      | notHSetMember z accSet = (accSeq :|> z, HSet.insert z accSet)
      | otherwise = (accSeq, accSet)

-- To preserve order, we must fold from the left
fromFoldable :: (Foldable f, Hashable a) => f a -> UniqueSeq a
fromFoldable = foldl' append (UnsafeUniqueSeq Seq.empty HSet.empty)

append :: (Hashable a) => UniqueSeq a -> a -> UniqueSeq a
append = flip (insertSeq (flip (:|>)))

prepend :: (Hashable a) => a -> UniqueSeq a -> UniqueSeq a
prepend = insertSeq (:<|)

insertSeq :: (Hashable a) => (a -> Seq a -> Seq a) -> a -> UniqueSeq a -> UniqueSeq a
insertSeq seqIns x useq@(UnsafeUniqueSeq seq set)
  | notHSetMember x set = UnsafeUniqueSeq (seqIns x seq) (HSet.insert x set)
  | otherwise = useq

notHSetMember :: (Hashable a) => a -> HashSet a -> Bool
notHSetMember x = not . HSet.member x
