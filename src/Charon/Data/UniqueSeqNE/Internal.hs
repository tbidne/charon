{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeqNE' type.
module Charon.Data.UniqueSeqNE.Internal
  ( UniqueSeqNE (MkUniqueSeqNE, UnsafeUniqueSeqNE),

    -- * Creation
    singleton,
    fromNonEmpty,
    unsafefromUniqueSeq,

    -- * Elimination
    toUniqueSeq,

    -- * Lookup
    member,

    -- * Operations
    prepend,
    append,
    union,
    map,
  )
where

import Charon.Data.UniqueSeq.Internal (UniqueSeq (UnsafeUniqueSeq))
import Charon.Prelude
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty))
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as NESeq

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

singleton :: (Hashable a) => a -> UniqueSeqNE a
singleton x = UnsafeUniqueSeqNE (NESeq.singleton x) (HSet.singleton x)

unsafefromUniqueSeq :: (HasCallStack) => UniqueSeq a -> UniqueSeqNE a
unsafefromUniqueSeq (UnsafeUniqueSeq Empty _) =
  error "Charon.Data.UniqueSeqNE.unsafefromUniqueSeq: empty UniqueSeq"
unsafefromUniqueSeq (UnsafeUniqueSeq (x :<| xs) set) =
  UnsafeUniqueSeqNE (x :<|| xs) set

toUniqueSeq :: UniqueSeqNE a -> UniqueSeq a
toUniqueSeq (UnsafeUniqueSeqNE (x :<|| xs) set) = UnsafeUniqueSeq (x :<| xs) set

union :: forall a. (Hashable a) => UniqueSeqNE a -> UniqueSeqNE a -> UniqueSeqNE a
union (UnsafeUniqueSeqNE (x :<|| xseq) _) (UnsafeUniqueSeqNE (y :<|| yseq) _) =
  UnsafeUniqueSeqNE (x :<|| newSeq) newSet
  where
    -- Given union (x : xs) (y : ys), we want (x : xs <> y : ys), eliminating
    -- duplicates. To do this, we iterate through (xs <> y : ys), building our
    -- new Seq/Set, only prepending x at the end.
    --
    -- see NOTE: [foldr vs. foldl']
    (newSeq, newSet) = foldr go (Seq.empty, HSet.singleton x) (xseq <> (y :<| yseq))

    go :: a -> (Seq a, HashSet a) -> (Seq a, HashSet a)
    go z (accSeq, accSet)
      | notHSetMember z accSet = (z :<| accSeq, HSet.insert z accSet)
      | otherwise = (accSeq, accSet)

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
      | notHSetMember y accSet = (y :<| accSeq, HSet.insert y accSet)
      | otherwise = (accSeq, accSet)
      where
        y = f z

-- NOTE: [foldr vs. foldl']
--
-- When building a UniqueSeqNE from some ordered Foldable, we want to
-- preserve order. Because we are dealing with NonEmpty, we hold onto the
-- head and prepend it when we are finished. Note that we need to add x to the
-- Set so that duplicates will not exist.
--
-- An alternative strategy would be to iterate though the entire structure
-- but use foldl' and append. We use the former strategy for now to keep it
-- consistent w/ UniqueSeq (i.e. laziness properties).

-- see NOTE: [foldr vs. foldl']
fromNonEmpty :: (Hashable a) => NonEmpty a -> UniqueSeqNE a
fromNonEmpty (x :| xs) =
  let (seq, set) = foldr f (Seq.empty, HSet.singleton x) xs
   in UnsafeUniqueSeqNE (x :<|| seq) set
  where
    f = insertSeq' (:<|)

insertSeq ::
  (Hashable a) =>
  (a -> Seq a -> NESeq a) ->
  a ->
  UniqueSeqNE a ->
  UniqueSeqNE a
insertSeq seqIns y useq@(UnsafeUniqueSeqNE (x :<|| xs) set)
  | notHSetMember y set = UnsafeUniqueSeqNE (seqIns y (x :<| xs)) (HSet.insert y set)
  | otherwise = useq

insertSeq' ::
  (Hashable a) =>
  (a -> Seq a -> Seq a) ->
  a ->
  (Seq a, HashSet a) ->
  (Seq a, HashSet a)
insertSeq' seqIns y (seq, set)
  | notHSetMember y set = (seqIns y seq, HSet.insert y set)
  | otherwise = (seq, set)

notHSetMember :: (Hashable a) => a -> HashSet a -> Bool
notHSetMember x = not . HSet.member x
