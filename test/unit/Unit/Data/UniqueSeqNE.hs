-- | Unit tests for Data.UniqueSeq
module Unit.Data.UniqueSeqNE
  ( tests,
  )
where

import Charon.Data.UniqueSeqNE ((∈), (∪), (⋃))
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Data.UniqueSeqNE.Internal
  ( UniqueSeqNE
      ( MkUniqueSeqNE,
        UnsafeUniqueSeqNE,
        seq,
        set
      ),
  )
import Data.HashSet qualified as HSet
import Data.Sequence.NonEmpty qualified as NESeq
import Hedgehog (PropertyT)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Unit.Prelude
import Utils qualified

tests :: TestTree
tests =
  testGroup
    "Data.UniqueSeqNE"
    [ invariantTests,
      lawsTests,
      specs
    ]

invariantTests :: TestTree
invariantTests =
  testGroup
    "General Invariants"
    [ toFromNEIsomorphism,
      fromToNEIsomorphism,
      fromToNEOrder,
      unionOrder,
      unionsOrder,
      mapInvariant,
      insertInvariant
    ]

toFromNEIsomorphism :: TestTree
toFromNEIsomorphism =
  testPropertyNamed "fromNonEmpty . toNonEmpty === id" "toFromNEIsomorphism" $ do
    property $ do
      useq <- forAll genUniqueSeq
      let xs = USeqNE.toNonEmpty useq
      annotateShow xs

      useq === USeqNE.fromNonEmpty xs

fromToNEIsomorphism :: TestTree
fromToNEIsomorphism =
  testPropertyNamed "toNonEmpty . fromNonEmpty === id for unique NonEmpty" "fromToNEIsomorphism" $ do
    property $ do
      xs <- forAll genUniqueNonEmpty
      let useq = USeqNE.fromNonEmpty xs
      annotateShow useq

      xs === USeqNE.toNonEmpty useq
      uniqseqInvariants useq

fromToNEOrder :: TestTree
fromToNEOrder =
  testPropertyNamed "toNonEmpty . fromNonEmpty preserves order" "fromToNEOrder" $ do
    property $ do
      origList <- forAll genNonEmpty
      let useq = USeqNE.fromNonEmpty origList
      annotateShow useq

      Utils.assertSameOrderNE origList (USeqNE.toNonEmpty useq)
      uniqseqInvariants useq

unionOrder :: TestTree
unionOrder =
  testPropertyNamed "union preserves order" "unionOrder" $ do
    property $ do
      as <- forAll genUniqueNonEmpty
      bs <- forAll genUniqueNonEmpty
      let a = USeqNE.fromNonEmpty as
          b = USeqNE.fromNonEmpty bs

          ab = a ∪ b
          ba = b ∪ a

      annotateShow a
      annotateShow b
      annotateShow ab
      annotateShow ba

      Utils.assertSameOrderNE (as <> bs) (USeqNE.toNonEmpty ab)
      Utils.assertSameOrderNE (bs <> as) (USeqNE.toNonEmpty ba)

      uniqseqInvariants ab
      uniqseqInvariants ba

unionsOrder :: TestTree
unionsOrder =
  testPropertyNamed "unions preserves order" "unionsOrder" $ do
    property $ do
      as <- forAll genUniqueNonEmpty
      bs <- forAll genUniqueNonEmpty
      cs <- forAll genUniqueNonEmpty
      let a = USeqNE.fromNonEmpty as
          b = USeqNE.fromNonEmpty bs
          c = USeqNE.fromNonEmpty cs
          abc = (⋃) (a :| [b, c])

      annotateShow abc

      Utils.assertSameOrderNE (as <> bs <> cs) (USeqNE.toNonEmpty abc)

      uniqseqInvariants abc

mapInvariant :: TestTree
mapInvariant =
  testPropertyNamed "map invariants" "mapInvariant" $ do
    property $ do
      useq <- forAll genUniqueSeq
      let useq' = USeqNE.map even useq
      uniqseqInvariants useq'

insertInvariant :: TestTree
insertInvariant =
  testPropertyNamed "insert invariants" "insertInvariant" $ do
    property $ do
      xs <- forAll genNonEmpty
      useqNE <- forAll genUniqueSeq
      let useqPrepend = foldr USeqNE.prepend useqNE xs
          useqAppend = foldl' USeqNE.append useqNE xs

      uniqseqInvariants useqPrepend
      uniqseqInvariants useqAppend

lawsTests :: TestTree
lawsTests =
  testGroup
    "Laws"
    [ semigroup,
      insertMember
    ]

semigroup :: TestTree
semigroup =
  testPropertyNamed "union is a semigroup" "semigroup" $ do
    property $ do
      a <- forAll genUniqueSeq
      b <- forAll genUniqueSeq
      c <- forAll genUniqueSeq

      annotate "Associativity"
      (a ∪ b) ∪ c === a ∪ (b ∪ c)

insertMember :: TestTree
insertMember =
  testPropertyNamed "x ∈ insert x useq" "insertMember" $ do
    property $ do
      useq <- forAll genUniqueSeq
      x <- forAll genInt

      let useqA = USeqNE.append useq x
          useqP = USeqNE.prepend x useq

      annotateShow useqA
      assert $ x ∈ useqA

      annotateShow useqP
      assert $ x ∈ useqP

      uniqseqInvariants useqA
      uniqseqInvariants useqP

uniqseqInvariants :: (Hashable a, Show a) => UniqueSeqNE a -> PropertyT IO ()
uniqseqInvariants useq = do
  foundRef <- liftIO $ newIORef HSet.empty
  seqAndSetSynced useq
  seqUnique foundRef useq

seqAndSetSynced :: (Hashable a, Show a) => UniqueSeqNE a -> PropertyT IO ()
seqAndSetSynced (MkUniqueSeqNE seq set) = do
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
  UniqueSeqNE a ->
  PropertyT IO ()
seqUnique foundRef (MkUniqueSeqNE seq _) = foldr go (pure ()) seq
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

genUniqueSeq :: Gen (UniqueSeqNE Int)
genUniqueSeq = USeqNE.fromNonEmpty <$> genNonEmpty

genUniqueNonEmpty :: Gen (NonEmpty Int)
genUniqueNonEmpty = do
  x :| xs <- genNonEmpty
  let (_, uniq) = foldl' go (HSet.singleton x, []) xs
  pure (x :| uniq)
  where
    go :: (HashSet Int, [Int]) -> Int -> (HashSet Int, [Int])
    go (found, acc) y
      | not (HSet.member y found) = (HSet.insert y found, y : acc)
      | otherwise = (found, acc)

genNonEmpty :: Gen (NonEmpty Int)
genNonEmpty = Gen.nonEmpty listRange genInt
  where
    listRange = Range.exponential 1 10_000

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
  let useq = USeqNE.fromNonEmpty xs

  expected @=? useq
  where
    xs :: NonEmpty Int
    xs = 0 :| [1, 0]

    expected =
      UnsafeUniqueSeqNE
        { seq = NESeq.fromList $ (0 :: Int) :| [1],
          set = HSet.fromList [0, 1, 0]
        }

unionPreservesOrder :: TestTree
unionPreservesOrder = testCase "Union preserves order" $ do
  expected @=? x ∪ y
  expected @=? y ∪ x
  where
    x =
      UnsafeUniqueSeqNE
        { seq = NESeq.fromList $ (0 :: Int) :| [],
          set = HSet.fromList [0]
        }

    y =
      UnsafeUniqueSeqNE
        { seq = NESeq.fromList $ (0 :: Int) :| [1],
          set = HSet.fromList [0, 1]
        }

    expected =
      UnsafeUniqueSeqNE
        { seq = NESeq.fromList $ (0 :: Int) :| [1],
          set = HSet.fromList [0, 1]
        }
