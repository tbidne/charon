-- | Provides the 'UniqueSeq' type.
module Charon.Data.UniqueSeq
  ( UniqueSeq (MkUniqueSeq),

    -- * Creation
    empty,
    singleton,
    Internal.fromFoldable,
    fromSet,

    -- * Lookup
    member,
    (∈),
    (∉),

    -- * Operations
    Internal.prepend,
    Internal.append,
    Internal.union,
    (∪),
    (⋃),
    map,
    (↤),
    (↦),

    -- * Display
    displayUSeqShow,
    displayUSeq,
  )
where

import Charon.Data.UniqueSeq.Internal
  ( UniqueSeq
      ( MkUniqueSeq,
        UnsafeUniqueSeq
      ),
  )
import Charon.Data.UniqueSeq.Internal qualified as Internal
import Charon.Prelude
import Data.Foldable (Foldable (toList))
import Data.HashSet qualified as HSet
import Data.Sequence qualified as Seq
import Data.Text qualified as T

empty :: UniqueSeq a
empty = UnsafeUniqueSeq Seq.empty HSet.empty

singleton :: (Hashable a) => a -> UniqueSeq a
singleton x = UnsafeUniqueSeq (Seq.singleton x) (HSet.singleton x)

member :: (Hashable a) => a -> UniqueSeq a -> Bool
member x (UnsafeUniqueSeq _ set) = HSet.member x set

map :: (Hashable b) => (a -> b) -> UniqueSeq a -> UniqueSeq b
map f (UnsafeUniqueSeq seq _) = UnsafeUniqueSeq newSeq newSet
  where
    (newSeq, newSet) = foldr go (Seq.empty, HSet.empty) seq
    go x (accSeq, accSet)
      | Internal.notHSetMember y accSet = (y :<| accSeq, HSet.insert y accSet)
      | otherwise = (accSeq, accSet)
      where
        y = f x

fromSet :: HashSet a -> UniqueSeq a
fromSet set = UnsafeUniqueSeq seq set
  where
    seq = foldr (flip (:|>)) Seq.empty set

displayUSeqShow :: (Show a) => UniqueSeq a -> Text
displayUSeqShow = displayUSeq showt

displayUSeq :: (a -> Text) -> UniqueSeq a -> Text
displayUSeq toText =
  T.intercalate ","
    . fmap toText
    . toList
    . view #seq

-- | Operator alias for 'union'. U+222A.
--
-- @since 0.1
(∪) :: (Hashable a) => UniqueSeq a -> UniqueSeq a -> UniqueSeq a
(∪) = Internal.union

infixl 6 ∪

-- | Fold over 'union'. U+22C3.
--
-- @since 0.1
(⋃) :: (Foldable f, Hashable a) => f (UniqueSeq a) -> UniqueSeq a
(⋃) = foldl' (∪) empty

-- | Operator alias for 'member'. U+2216.
--
-- @since 0.1
(∈) :: (Hashable a) => a -> UniqueSeq a -> Bool
(∈) = member

infix 4 ∈

-- | Negation of '(∈)'. U+2209.
--
-- @since 0.1
(∉) :: (Hashable a) => a -> UniqueSeq a -> Bool
(∉) x = not . (∈) x

infix 4 ∉

-- | Flipped '(↤)'. U+21A6.
--
-- @since 0.1
(↦) :: (Hashable b) => UniqueSeq a -> (a -> b) -> UniqueSeq b
(↦) = flip (↤)

infix 3 ↦

-- | Operator alias for 'map'. U+21A4.
--
-- @since 0.1
(↤) :: (Hashable b) => (a -> b) -> UniqueSeq a -> UniqueSeq b
(↤) = map

infix 3 ↤
