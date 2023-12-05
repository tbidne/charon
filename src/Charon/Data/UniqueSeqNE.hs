-- | Provides the 'UniqueSeqNE' type.
module Charon.Data.UniqueSeqNE
  ( UniqueSeqNE (MkUniqueSeqNE),

    -- * Creation
    singleton,
    fromNonEmpty,
    unsafefromUniqueSeq,

    -- * Elimination
    toNonEmpty,
    toUniqueSeq,

    -- * Lookup
    member,

    -- * Operations
    prepend,
    append,
    Internal.union,
    map,
  )
where

import Charon.Data.UniqueSeq.Internal (UniqueSeq (UnsafeUniqueSeq))
import Charon.Data.UniqueSeqNE.Internal
  ( UniqueSeqNE
      ( MkUniqueSeqNE,
        UnsafeUniqueSeqNE
      ),
  )
import Charon.Data.UniqueSeqNE.Internal qualified as Internal
import Charon.Prelude
import Data.Foldable (Foldable (toList))
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq

singleton :: (Hashable a) => a -> UniqueSeqNE a
singleton x = UnsafeUniqueSeqNE (NESeq.singleton x) (HSet.singleton x)

unsafefromUniqueSeq :: (HasCallStack) => UniqueSeq a -> UniqueSeqNE a
unsafefromUniqueSeq (UnsafeUniqueSeq Empty _) =
  error "Charon.Data.UniqueSeqNE.unsafefromUniqueSeq: empty UniqueSeq"
unsafefromUniqueSeq (UnsafeUniqueSeq (x :<| xs) set) =
  UnsafeUniqueSeqNE (x :<|| xs) set

toUniqueSeq :: UniqueSeqNE a -> UniqueSeq a
toUniqueSeq (UnsafeUniqueSeqNE (x :<|| xs) set) = UnsafeUniqueSeq (x :<| xs) set

member :: (Hashable a) => a -> UniqueSeqNE a -> Bool
member y (UnsafeUniqueSeqNE _ useq) = HSet.member y useq

append :: (Hashable a) => UniqueSeqNE a -> a -> UniqueSeqNE a
append = flip (insertSeq (flip (:||>)))

prepend :: (Hashable a) => a -> UniqueSeqNE a -> UniqueSeqNE a
prepend = insertSeq (:<||)

map :: (Hashable b) => (a -> b) -> UniqueSeqNE a -> UniqueSeqNE b
map f (UnsafeUniqueSeqNE (x :<|| seq) _) = UnsafeUniqueSeqNE (f x :<|| newSeq) newSet
  where
    (newSeq, newSet) = foldr go (Seq.empty, HSet.singleton (f x)) seq
    go z (accSeq, accSet)
      | Internal.notHSetMember y accSet = (y :<| accSeq, HSet.insert y accSet)
      | otherwise = (accSeq, accSet)
      where
        y = f z

-- When building a UniqueSeqNE from some ordered Foldable, we want to
-- preserve order. Because we are dealing with NonEmpty, we hold onto the
-- head and prepend it when we are finished. Note that we need to add x to the
-- Set so that duplicates will not exist.
fromNonEmpty :: (Hashable a) => NonEmpty a -> UniqueSeqNE a
fromNonEmpty (x :| xs) = UnsafeUniqueSeqNE (x :<|| seq) set
  where
    -- To preserve order, we must fold from the left
    (seq, set) = foldl' f (Seq.empty, HSet.singleton x) xs
    f = flip (insertSeq' (flip (:|>)))

toNonEmpty :: UniqueSeqNE a -> NonEmpty a
toNonEmpty useq = x :| toList xs
  where
    (x :<|| xs) = view #seq useq

insertSeq ::
  (Hashable a) =>
  (a -> Seq a -> NESeq a) ->
  a ->
  UniqueSeqNE a ->
  UniqueSeqNE a
insertSeq seqIns y useq@(UnsafeUniqueSeqNE (x :<|| xs) set)
  | Internal.notHSetMember y set = UnsafeUniqueSeqNE (seqIns y (x :<| xs)) (HSet.insert y set)
  | otherwise = useq

insertSeq' ::
  (Hashable a) =>
  (a -> Seq a -> Seq a) ->
  a ->
  (Seq a, HashSet a) ->
  (Seq a, HashSet a)
insertSeq' seqIns y (seq, set)
  | Internal.notHSetMember y set = (seqIns y seq, HSet.insert y set)
  | otherwise = (seq, set)
