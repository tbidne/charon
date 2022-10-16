{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeq' type.
--
-- @since 0.1
module SafeRm.Data.UniqueSeq
  ( UniqueSeq (MkUniqueSeq),
    fromFoldable,
    fromSet,
  )
where

import Containers.Class
  ( CMap (CMapC),
    Empty (empty),
    Member (member),
    Sequenced (SElem, append, prepend),
    Union (union),
    cmap,
  )
import GHC.Exts (IsList (Item, fromList, toList))
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)
import SafeRm.Prelude

-- | Like 'Seq' but with the guarantee that all elements are unique.
--
-- @since 0.1
data UniqueSeq a = UnsafeUniqueSeq
  { seq :: !(Seq a),
    set :: !(HashSet a)
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Foldable UniqueSeq where
  foldr f x (UnsafeUniqueSeq seq _) = foldr f x seq

-- | @since 0.1
pattern MkUniqueSeq :: Seq a -> HashSet a -> UniqueSeq a
pattern MkUniqueSeq seq set <- UnsafeUniqueSeq seq set

{-# COMPLETE MkUniqueSeq #-}

-- | @since 0.1
instance
  (k ~ A_Getter, b ~ Seq a, c ~ Seq a) =>
  LabelOptic "seq" k (UniqueSeq a) (UniqueSeq a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeq seq _) -> seq)

-- | @since 0.1
instance
  (k ~ A_Getter, b ~ HashSet a, c ~ HashSet a) =>
  LabelOptic "set" k (UniqueSeq a) (UniqueSeq a) b c
  where
  labelOptic = to (\(UnsafeUniqueSeq _ set) -> set)

-- | @since 0.1
instance Hashable a => Semigroup (UniqueSeq a) where
  (<>) = (∪)

-- | @since 0.1
instance Hashable a => Monoid (UniqueSeq a) where
  mempty = (∅)

-- | @since 0.1
instance Empty (UniqueSeq a) where
  empty = UnsafeUniqueSeq (∅) (∅)

-- | @since 0.1
instance Hashable a => Union (UniqueSeq a) where
  union (UnsafeUniqueSeq xseq xset) (UnsafeUniqueSeq yseq yset) =
    UnsafeUniqueSeq (view _2 (foldr go ((∅), (∅)) (xseq <> yseq))) (xset ∪ yset)
    where
      go :: a -> (HashSet a, Seq a) -> (HashSet a, Seq a)
      go z (found, acc)
        | z ∉ found = (z ⟇ found, acc ⋗ z)
        | otherwise = (found, acc)

-- | @since 0.1
instance Hashable a => Member (UniqueSeq a) where
  type MElem (UniqueSeq a) = a
  member x (UnsafeUniqueSeq _ set) = x ∈ set

-- | @since 0.1
instance Hashable a => Sequenced (UniqueSeq a) where
  type SElem (UniqueSeq a) = a
  append = flip (insertSeq (flip (|>)))
  prepend = insertSeq (<|)

-- | @since 0.1
instance CMap UniqueSeq where
  type CMapC UniqueSeq a = Hashable a
  cmap f (UnsafeUniqueSeq seq set) = UnsafeUniqueSeq (φ f seq) (φ f set)

-- | @since 0.1
instance Hashable a => IsList (UniqueSeq a) where
  type Item (UniqueSeq a) = a
  toList (UnsafeUniqueSeq seq _) = toList seq

  -- TODO: should verify that order is maintained
  fromList = fromFoldable

-- | @since 0.1
insertSeq :: Hashable a => (a -> Seq a -> Seq a) -> a -> UniqueSeq a -> UniqueSeq a
insertSeq seqIns x useq@(UnsafeUniqueSeq seq set)
  | x ∉ useq = UnsafeUniqueSeq (seqIns x seq) (x ⟇ set)
  | otherwise = useq

-- | @since 0.1
fromFoldable :: (Foldable f, Hashable a) => f a -> UniqueSeq a
fromFoldable = foldr (flip (⋗)) (∅)

-- | @since 0.1
fromSet :: HashSet a -> UniqueSeq a
fromSet set = UnsafeUniqueSeq seq set
  where
    seq = foldr (flip (⋗)) (∅) set
