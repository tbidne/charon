{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeqNE' type.
module Charon.Data.UniqueSeqNE
  ( UniqueSeqNE (MkUniqueSeqNE),

    -- * Creation
    singleton,
    fromNonEmpty,

    -- * Lookup
    member,
    unsafefromUniqueSeq,

    -- * Operations
    prepend,
    append,
    union,
    map,
  )
where

import Charon.Data.UniqueSeq qualified as UniqueSeq
import Charon.Data.UniqueSeq.Internal (UniqueSeq (UnsafeUniqueSeq))
import Charon.Prelude

-- | Like 'UniqueSeq' except carries the invariant that it is non-empty.
newtype UniqueSeqNE a = UnsafeUniqueSeqNE {unUniqueSeqNE :: UniqueSeq a}
  deriving stock (Eq, Show)
  deriving (Foldable) via UniqueSeq
  deriving (Semigroup) via (UniqueSeq a)

pattern MkUniqueSeqNE :: Seq a -> HashSet a -> UniqueSeqNE a
pattern MkUniqueSeqNE seq set <- UnsafeUniqueSeqNE (UnsafeUniqueSeq seq set)

{-# COMPLETE MkUniqueSeqNE #-}

instance
  (k ~ A_Getter, b ~ Seq a, c ~ Seq a) =>
  LabelOptic "seq" k (UniqueSeqNE a) (UniqueSeqNE a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeqNE (UnsafeUniqueSeq seq _)) -> seq)

instance
  (k ~ A_Getter, b ~ HashSet a, c ~ HashSet a) =>
  LabelOptic "set" k (UniqueSeqNE a) (UniqueSeqNE a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeqNE (UnsafeUniqueSeq _ set)) -> set)

singleton :: (Hashable a) => a -> UniqueSeqNE a
singleton = UnsafeUniqueSeqNE . UniqueSeq.singleton

unsafefromUniqueSeq :: (HasCallStack) => UniqueSeq a -> UniqueSeqNE a
unsafefromUniqueSeq useq =
  if null useq
    then error "Charon.Data.UniqueSeqNE.unsafefromUniqueSeq: empty UniqueSeq"
    else UnsafeUniqueSeqNE useq

union :: forall a. (Hashable a) => UniqueSeqNE a -> UniqueSeqNE a -> UniqueSeqNE a
union (UnsafeUniqueSeqNE useq1) (UnsafeUniqueSeqNE useq2) =
  UnsafeUniqueSeqNE $ UniqueSeq.union useq1 useq2

member :: (Hashable a) => a -> UniqueSeqNE a -> Bool
member x (UnsafeUniqueSeqNE useq) = UniqueSeq.member x useq

append :: (Hashable a) => UniqueSeqNE a -> a -> UniqueSeqNE a
append (UnsafeUniqueSeqNE useq) = UnsafeUniqueSeqNE . UniqueSeq.append useq

prepend :: (Hashable a) => a -> UniqueSeqNE a -> UniqueSeqNE a
prepend x (UnsafeUniqueSeqNE useq) = UnsafeUniqueSeqNE $ UniqueSeq.prepend x useq

map :: (Hashable b) => (a1 -> b) -> UniqueSeqNE a1 -> UniqueSeqNE b
map f (UnsafeUniqueSeqNE useq) = UnsafeUniqueSeqNE $ UniqueSeq.map f useq

fromNonEmpty :: (Hashable a) => NonEmpty a -> UniqueSeqNE a
fromNonEmpty xs = UnsafeUniqueSeqNE $ UniqueSeq.fromFoldable xs
