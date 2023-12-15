-- | Unit tests for Data.UniqueSeq
module Unit.Data.UniqueSeq
  ( tests,
  )
where

import Charon.Data.UniqueSeq ((∈), (∪), (⋃))
import Charon.Data.UniqueSeq qualified as USeq
import Charon.Data.UniqueSeq.Internal
  ( UniqueSeq
      ( MkUniqueSeq,
        UnsafeUniqueSeq,
        seq,
        set
      ),
  )
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
      lawsTests,
      specs
    ]

invariantTests :: TestTree
invariantTests =
  testGroup
    "General Invariants"
    [ toFromListIsomorphism,
      fromToListIsomorphism,
      fromToListOrder,
      fromFoldableOrder,
      unionOrder,
      unionsOrder,
      mapInvariant,
      insertInvariant
    ]

toFromListIsomorphism :: TestTree
toFromListIsomorphism =
  testPropertyNamed "fromList . toList === id" "toFromListIsomorphism" $ do
    property $ do
      useq <- forAll genUniqueSeq
      let xs = toList useq
      annotateShow xs

      useq === fromList xs

fromToListIsomorphism :: TestTree
fromToListIsomorphism =
  testPropertyNamed "toList . fromList === id for unique list" "fromToListIsomorphism" $ do
    property $ do
      xs <- forAll genUniqueList
      let useq = fromList @(UniqueSeq Int) xs
      annotateShow useq

      xs === toList useq
      uniqseqInvariants useq

fromToListOrder :: TestTree
fromToListOrder =
  testPropertyNamed "toList . fromList preserves order" "fromToListOrder" $ do
    property $ do
      xs <- forAll genList
      let useq = fromList @(UniqueSeq Int) xs
      annotateShow useq

      Utils.assertSameOrder xs (toList useq)
      uniqseqInvariants useq

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
      as <- forAll genUniqueList
      bs <- forAll genUniqueList
      let a = USeq.fromFoldable as
          b = USeq.fromFoldable bs

          ab = a ∪ b
          ba = b ∪ a

      annotateShow a
      annotateShow b
      annotateShow ab
      annotateShow ba

      Utils.assertSameOrder (as ++ bs) (toList ab)
      Utils.assertSameOrder (bs ++ as) (toList ba)

      uniqseqInvariants ab
      uniqseqInvariants ba

unionsOrder :: TestTree
unionsOrder =
  testPropertyNamed "unions preserves order" "unionsOrder" $ do
    property $ do
      as <- forAll genUniqueList
      bs <- forAll genUniqueList
      cs <- forAll genUniqueList
      let a = USeq.fromFoldable as
          b = USeq.fromFoldable bs
          c = USeq.fromFoldable cs
          abc = (⋃) [a, b, c]

      annotateShow abc

      Utils.assertSameOrder (as <> bs <> cs) (toList abc)

      uniqseqInvariants abc

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
      a === a ∪ USeq.empty
      a === USeq.empty ∪ a

      annotate "Associativity"
      (a ∪ b) ∪ c === a ∪ (b ∪ c)

insertMember :: TestTree
insertMember =
  testPropertyNamed "x ∈ insert x useq" "insertMember" $ do
    property $ do
      useq <- forAll genUniqueSeq
      x <- forAll genInt

      let useqA = USeq.append useq x
          useqP = USeq.prepend x useq

      annotateShow useqA
      assert $ x ∈ useqA

      annotateShow useqP
      assert $ x ∈ useqP

      uniqseqInvariants useqA
      uniqseqInvariants useqP

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
    listRange = Range.exponential 0 10_000

genInt :: Gen Int
genInt = Gen.integral intRange
  where
    intRange = Range.exponentialFrom 0 0 1_000

specs :: TestTree
specs =
  testGroup
    "Specs"
    [ duplicatePreservesOrder,
      unionPreservesOrder
    ]

duplicatePreservesOrder :: TestTree
duplicatePreservesOrder = testCase "Duplicate preserves order" $ do
  let useq = USeq.fromFoldable xs

  expected @=? useq
  where
    xs :: [Int]
    xs = [0, 1, 0]

    expected =
      UnsafeUniqueSeq
        { seq = fromList [0, 1],
          set = HSet.fromList xs
        }

unionPreservesOrder :: TestTree
unionPreservesOrder = testCase "Union preserves order" $ do
  expected @=? x ∪ y
  expected @=? y ∪ x
  where
    x =
      UnsafeUniqueSeq
        { seq = fromList [0 :: Int],
          set = HSet.fromList [0]
        }

    y =
      UnsafeUniqueSeq
        { seq = fromList [0, 1 :: Int],
          set = HSet.fromList [0, 1]
        }

    expected =
      UnsafeUniqueSeq
        { seq = fromList [0, 1 :: Int],
          set = HSet.fromList [0, 1]
        }
