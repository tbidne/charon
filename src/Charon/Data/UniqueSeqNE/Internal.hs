{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeqNE' type.
module Charon.Data.UniqueSeqNE.Internal
  ( -- * Type
    UniqueSeqNE (MkUniqueSeqNE, UnsafeUniqueSeqNE),

    -- * Operations
    union,

    -- * Utils
    notHSetMember,
  )
where

import Charon.Prelude
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq

-- | Like 'UniqueSeq' except carries the invariant that it is non-empty.
data UniqueSeqNE a = UnsafeUniqueSeqNE
  { seq :: NESeq a,
    set :: HashSet a
  }
  deriving stock (Eq, Show)

pattern MkUniqueSeqNE :: NESeq a -> HashSet a -> UniqueSeqNE a
pattern MkUniqueSeqNE seq set <- UnsafeUniqueSeqNE seq set

{-# COMPLETE MkUniqueSeqNE #-}

instance Foldable UniqueSeqNE where
  foldr f x (UnsafeUniqueSeqNE seq _) = foldr f x seq

instance (Hashable a) => Semigroup (UniqueSeqNE a) where
  (<>) = union

instance
  (k ~ A_Getter, b ~ NESeq a, c ~ NESeq a) =>
  LabelOptic "seq" k (UniqueSeqNE a) (UniqueSeqNE a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeqNE seq _) -> seq)

instance
  (k ~ A_Getter, b ~ HashSet a, c ~ HashSet a) =>
  LabelOptic "set" k (UniqueSeqNE a) (UniqueSeqNE a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeqNE _ set) -> set)

union :: forall a. (Hashable a) => UniqueSeqNE a -> UniqueSeqNE a -> UniqueSeqNE a
union (UnsafeUniqueSeqNE (x :<|| xseq) _) (UnsafeUniqueSeqNE (y :<|| yseq) _) =
  UnsafeUniqueSeqNE (x :<|| newSeq) newSet
  where
    -- Given union (x : xs) (y : ys), we want (x : xs <> y : ys), eliminating
    -- duplicates. To do this, we iterate through (xs <> y : ys), building our
    -- new Seq/Set, only prepending x at the end.
    --
    -- see NOTE: [UniqueSeqNE foldr vs. foldl']
    (newSeq, newSet) = foldr go (Seq.empty, HSet.singleton x) (xseq <> (y :<| yseq))

    go :: a -> (Seq a, HashSet a) -> (Seq a, HashSet a)
    go z (accSeq, accSet)
      | notHSetMember z accSet = (z :<| accSeq, HSet.insert z accSet)
      | otherwise = (accSeq, accSet)

notHSetMember :: (Hashable a) => a -> HashSet a -> Bool
notHSetMember x = not . HSet.member x
