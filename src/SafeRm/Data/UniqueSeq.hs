{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'UniqueSeq' type.
--
-- @since 0.1
module SafeRm.Data.UniqueSeq
  ( UniqueSeq (MkUniqueSeq),

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

import Data.HashSet qualified as HSet
import GHC.Exts (IsList (Item, fromList, toList))
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)
import SafeRm.Prelude

-- | Like 'Seq' but with the guarantee that all elements are unique.
-- Note that the 'CMap' instance does _not_ preserve its structure when
-- the lifted function is not injective.
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
  (<>) = union

-- | @since 0.1
instance Hashable a => Monoid (UniqueSeq a) where
  mempty = UnsafeUniqueSeq [] HSet.empty

-- | @since 0.1
empty :: UniqueSeq a
empty = UnsafeUniqueSeq [] HSet.empty

-- | @since 0.1
singleton :: Hashable a => a -> UniqueSeq a
singleton x = UnsafeUniqueSeq [x] (HSet.singleton x)

-- | @since 0.1
union :: forall a. Hashable a => UniqueSeq a -> UniqueSeq a -> UniqueSeq a
union (UnsafeUniqueSeq xseq _) (UnsafeUniqueSeq yseq _) =
  UnsafeUniqueSeq newSeq newSet
  where
    (newSeq, newSet) = foldr go ([], HSet.empty) (xseq <> yseq)
    go :: a -> (Seq a, HashSet a) -> (Seq a, HashSet a)
    go z (accSeq, accSet)
      | notHSetMember z accSet = (z <| accSeq, HSet.insert z accSet)
      | otherwise = (accSeq, accSet)

-- | @since 0.1
member :: Hashable a => a -> UniqueSeq a -> Bool
member x (UnsafeUniqueSeq _ set) = HSet.member x set

-- | @since 0.1
append :: Hashable a => UniqueSeq a -> a -> UniqueSeq a
append = flip (insertSeq (flip (|>)))

-- | @since 0.1
prepend :: Hashable a => a -> UniqueSeq a -> UniqueSeq a
prepend = insertSeq (<|)

-- | @since 0.1
map :: Hashable b => (a1 -> b) -> UniqueSeq a1 -> UniqueSeq b
map f (UnsafeUniqueSeq seq _) = UnsafeUniqueSeq newSeq newSet
  where
    (newSeq, newSet) = foldr go ([], HSet.empty) seq
    go x (accSeq, accSet)
      | notHSetMember y accSet = (y <| accSeq, HSet.insert y accSet)
      | otherwise = (accSeq, accSet)
      where
        y = f x

-- | @since 0.1
instance Hashable a => IsList (UniqueSeq a) where
  type Item (UniqueSeq a) = a
  toList (UnsafeUniqueSeq seq _) = toList seq
  fromList = fromFoldable

-- | @since 0.1
insertSeq :: Hashable a => (a -> Seq a -> Seq a) -> a -> UniqueSeq a -> UniqueSeq a
insertSeq seqIns x useq@(UnsafeUniqueSeq seq set)
  | notHSetMember x set = UnsafeUniqueSeq (seqIns x seq) (HSet.insert x set)
  | otherwise = useq

-- | @since 0.1
fromFoldable :: (Foldable f, Hashable a) => f a -> UniqueSeq a
fromFoldable = foldr prepend (UnsafeUniqueSeq [] HSet.empty)

-- | @since 0.1
fromSet :: HashSet a -> UniqueSeq a
fromSet set = UnsafeUniqueSeq seq set
  where
    seq = foldr (flip (|>)) [] set

notHSetMember :: Hashable a => a -> HashSet a -> Bool
notHSetMember x = not . HSet.member x
