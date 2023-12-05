-- | Unit tests for Data.UniqueSeq
module Unit.Data.UniqueSeq
  ( tests,
  )
where

import Charon.Data.UniqueSeq (UniqueSeq (MkUniqueSeq))
import Charon.Data.UniqueSeq qualified as USeq
import Data.HashSet qualified as HSet
import Data.Sequence (Seq (Empty))
import GHC.Exts (IsList (fromList, toList))
import Hedgehog (PropertyT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unit.Prelude
import Utils qualified

tests :: TestTree
tests =
  testGroup
    "Data.UniqueSeq"
    [ invariantTests,
      lawsTests
    ]

invariantTests :: TestTree
invariantTests =
  testGroup
    "General Invariants"
    [ isListIsomorphism,
      isListOrder,
      fromFoldableOrder,
      unionOrder,
      mapInvariant,
      insertInvariant
    ]

isListIsomorphism :: TestTree
isListIsomorphism =
  testPropertyNamed "fromList . toList === id" "isListIsomorphism" $ do
    property $ do
      xs <- forAll genUniqueSeq
      let useq = toList xs
      annotateShow useq
      xs === fromList useq

isListOrder :: TestTree
isListOrder =
  testPropertyNamed "toList . fromList preserves order" "isListOrder" $ do
    property $ do
      origList <- forAll genUniqueList
      let useq = fromList @(UniqueSeq Int) origList
      annotateShow useq

      Utils.assertSameOrder origList (toList useq)

fromFoldableOrder :: TestTree
fromFoldableOrder =
  testPropertyNamed "fromFoldable preserves order" "fromFoldableOrder" $ do
    property $ do
      xs <- forAll genUniqueList
      let useq@(MkUniqueSeq seq _) = USeq.fromFoldable xs

      annotateShow seq
      sameOrder xs seq

      uniqseqInvariants useq
  where
    sameOrder [] Empty = pure ()
    sameOrder (x : _) Empty = annotateShow x *> failure
    sameOrder [] (y :<| _) = annotateShow y *> failure
    sameOrder (x : xs) (y :<| ys) = do
      x === y
      sameOrder xs ys

unionOrder :: TestTree
unionOrder =
  testPropertyNamed "union preserves order" "unionOrder" $ do
    property $ do
      xs <- forAll genUniqueList
      ys <- forAll genUniqueList
      let xuseq = USeq.fromFoldable xs
          yuseq = USeq.fromFoldable ys

          uxy = USeq.union xuseq yuseq
          uyx = USeq.union yuseq xuseq

      annotateShow xuseq
      annotateShow yuseq
      annotateShow uxy
      annotateShow uyx

      Utils.assertSameOrder (xs ++ ys) (toList uxy)
      Utils.assertSameOrder (ys ++ xs) (toList uyx)

      uniqseqInvariants uxy
      uniqseqInvariants uyx

mapInvariant :: TestTree
mapInvariant =
  testPropertyNamed "map invariants" "mapInvariant" $ do
    property $ do
      useq <- forAll genUniqueSeq
      let useq' = USeq.map even useq
      uniqseqInvariants useq'

insertInvariant :: TestTree
insertInvariant =
  testPropertyNamed "insert invariants" "insertInvariant" $ do
    property $ do
      xs <- forAll genList
      let useqPrepend = foldr USeq.prepend USeq.empty xs
          useqAppend = foldl' USeq.append USeq.empty xs

      uniqseqInvariants useqPrepend
      uniqseqInvariants useqAppend

lawsTests :: TestTree
lawsTests =
  testGroup
    "Laws"
    [ monoid,
      insertMember
    ]

monoid :: TestTree
monoid =
  testPropertyNamed "union is a monoid" "monoid" $ do
    property $ do
      a <- forAll genUniqueSeq
      b <- forAll genUniqueSeq
      c <- forAll genUniqueSeq

      annotate "Identity"
      a === a `USeq.union` USeq.empty
      a === USeq.empty `USeq.union` a

      annotate "Associativity"
      (a `USeq.union` b) `USeq.union` c === a `USeq.union` (b `USeq.union` c)

insertMember :: TestTree
insertMember =
  testPropertyNamed "x ∈ insert x useq" "insertMember" $ do
    property $ do
      useq <- forAll genUniqueSeq
      x <- forAll genInt

      let useqA = USeq.append useq x
          useqP = USeq.prepend x useq

      annotateShow useqA
      assert $ USeq.member x useqA

      annotateShow useqP
      assert $ USeq.member x useqP

uniqseqInvariants :: (Hashable a, Show a) => UniqueSeq a -> PropertyT IO ()
uniqseqInvariants useq = do
  foundRef <- liftIO $ newIORef HSet.empty
  seqAndSetSynced useq
  seqUnique foundRef useq

seqAndSetSynced :: (Hashable a, Show a) => UniqueSeq a -> PropertyT IO ()
seqAndSetSynced (MkUniqueSeq seq set) = do
  annotateShow seq
  annotateShow set
  -- same size
  length seq === length set

  -- all seq in set
  for_ seq $ \x -> do
    annotateShow x
    assert $ HSet.member x set

seqUnique ::
  forall a.
  (Hashable a, Show a) =>
  IORef (HashSet a) ->
  UniqueSeq a ->
  PropertyT IO ()
seqUnique foundRef (MkUniqueSeq seq _) = foldr go (pure ()) seq
  where
    go :: a -> PropertyT IO () -> PropertyT IO ()
    go x acc = do
      found <- liftIO $ readIORef foundRef
      if HSet.member x found
        then do
          annotate "Found duplicate"
          annotateShow x
          failure
        else do
          liftIO $ modifyIORef' foundRef (HSet.insert x)
          acc

genUniqueSeq :: Gen (UniqueSeq Int)
genUniqueSeq = fromList <$> genList

genUniqueList :: Gen [Int]
genUniqueList = do
  xs <- genList
  let (_, uniq) = foldl' go (HSet.empty, []) xs
  pure uniq
  where
    go :: (HashSet Int, [Int]) -> Int -> (HashSet Int, [Int])
    go (found, acc) y
      | not (HSet.member y found) = (HSet.insert y found, y : acc)
      | otherwise = (found, acc)

genList :: Gen [Int]
genList = Gen.list listRange genInt
  where
    listRange = Range.exponential 1 10_000

genInt :: Gen Int
genInt = Gen.integral intRange
  where
    intRange = Range.exponentialFrom 0 0 1_000
