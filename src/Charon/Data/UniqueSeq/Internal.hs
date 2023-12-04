{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeq' type.
module Charon.Data.UniqueSeq.Internal
  ( UniqueSeq (MkUniqueSeq, UnsafeUniqueSeq),

    -- * Creation
    empty,
    singleton,
    fromFoldable,
    fromSet,

    -- * Lookup
    member,

    -- * Operations
    prepend,
    append,
    union,
    map,
  )
where

import Charon.Prelude
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import GHC.Exts (IsList (Item, fromList, toList))

-- | Like 'Seq' but with the guarantee that all elements are unique.
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

empty :: UniqueSeq a
empty = UnsafeUniqueSeq Seq.empty HSet.empty

singleton :: (Hashable a) => a -> UniqueSeq a
singleton x = UnsafeUniqueSeq (Seq.singleton x) (HSet.singleton x)

union :: forall a. (Hashable a) => UniqueSeq a -> UniqueSeq a -> UniqueSeq a
union (UnsafeUniqueSeq xseq _) (UnsafeUniqueSeq yseq _) =
  UnsafeUniqueSeq newSeq newSet
  where
    (newSeq, newSet) = foldr go (Seq.empty, HSet.empty) (xseq <> yseq)
    go :: a -> (Seq a, HashSet a) -> (Seq a, HashSet a)
    go z (accSeq, accSet)
      | notHSetMember z accSet = (z :<| accSeq, HSet.insert z accSet)
      | otherwise = (accSeq, accSet)

member :: (Hashable a) => a -> UniqueSeq a -> Bool
member x (UnsafeUniqueSeq _ set) = HSet.member x set

append :: (Hashable a) => UniqueSeq a -> a -> UniqueSeq a
append = flip (insertSeq (flip (:|>)))

prepend :: (Hashable a) => a -> UniqueSeq a -> UniqueSeq a
prepend = insertSeq (:<|)

map :: (Hashable b) => (a -> b) -> UniqueSeq a -> UniqueSeq b
map f (UnsafeUniqueSeq seq _) = UnsafeUniqueSeq newSeq newSet
  where
    (newSeq, newSet) = foldr go (Seq.empty, HSet.empty) seq
    go x (accSeq, accSet)
      | notHSetMember y accSet = (y :<| accSeq, HSet.insert y accSet)
      | otherwise = (accSeq, accSet)
      where
        y = f x

instance (Hashable a) => IsList (UniqueSeq a) where
  type Item (UniqueSeq a) = a
  toList (UnsafeUniqueSeq seq _) = toList seq
  fromList = fromFoldable

insertSeq :: (Hashable a) => (a -> Seq a -> Seq a) -> a -> UniqueSeq a -> UniqueSeq a
insertSeq seqIns x useq@(UnsafeUniqueSeq seq set)
  | notHSetMember x set = UnsafeUniqueSeq (seqIns x seq) (HSet.insert x set)
  | otherwise = useq

fromFoldable :: (Foldable f, Hashable a) => f a -> UniqueSeq a
fromFoldable = foldr prepend (UnsafeUniqueSeq Seq.empty HSet.empty)

fromSet :: HashSet a -> UniqueSeq a
fromSet set = UnsafeUniqueSeq seq set
  where
    seq = foldr (flip (:|>)) Seq.empty set

notHSetMember :: (Hashable a) => a -> HashSet a -> Bool
notHSetMember x = not . HSet.member x
