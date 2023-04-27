module Unit.Data.PathData
  ( tests,
  )
where

import Data.Fixed (Fixed (MkFixed))
import Data.Time (LocalTime (LocalTime), TimeOfDay (..))
import GHC.Real ((^))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import SafeRm.Data.PathData.Default qualified as Default
import SafeRm.Data.PathData.Fdo qualified as Fdo
import SafeRm.Data.PathType (PathType (..))
import SafeRm.Data.Paths (PathI (..))
import SafeRm.Data.Serialize (Serialize (..))
import SafeRm.Data.Timestamp (Timestamp (..))
import SafeRm.Data.Timestamp qualified as Timestamp
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.PathData"
    [ defaultTests,
      fdoTests
    ]

defaultTests :: TestTree
defaultTests =
  testGroup
    "Default"
    [ serializeRoundtripSpecsDefault,
      serializeRoundtripPropDefault
    ]

serializeRoundtripSpecsDefault :: TestTree
serializeRoundtripSpecsDefault = testCase "decode . encode ~ id (specs)" $ do
  ts <- Timestamp.fromText "1858-11-17T00:00:00"

  let (pd1, encoded1) = mkPd ts "\NUL" "\t"
  "[Trash Info]\nPath=\t\nDeletionDate=1858-11-17T00:00:00\nSize=0\nType=d\n" @=? encoded1
  Right pd1 @=? decode (MkPathI "\NUL") encoded1

  let (pd2, encoded2) = mkPd ts "\NUL" "\n"
  "[Trash Info]\nPath=\n\nDeletionDate=1858-11-17T00:00:00\nSize=0\nType=d\n" @=? encoded2
  Right pd2 @=? decode (MkPathI "\NUL") encoded2
  where
    mkPd ts fileName' originalPath' =
      let pd =
            Default.UnsafePathData
              { pathType = PathTypeDirectory,
                fileName = MkPathI fileName',
                originalPath = MkPathI originalPath',
                size = MkBytes 0,
                created = ts
              }
       in (pd, encode pd)

serializeRoundtripPropDefault :: TestTree
serializeRoundtripPropDefault =
  testPropertyNamed "decode . encode ~ id (prop)" "serializeRoundtripPropDefault" $ do
    property $ do
      pathData@(Default.UnsafePathData _ fileName _ _ _) <- forAll genDefaultPathData
      -- NOTE:
      -- Two caveats on injectivity:
      --
      -- 1. The fileName is thrown away. However this is intentional as we
      -- want it in the actual file name only (otherwise it is redundant).
      --
      -- We can thus think of the actual injection in terms of the implicit
      -- file name that is created by the encoding.
      --
      -- 2. We lose precision in the created timestamp, as we encode to second
      -- precision whereas the timestamp field technically has picosecond
      -- resolution. This is acceptable, as we do not care about
      -- precision > second.
      let encoded = encode pathData
          decoded = decode fileName encoded

      annotateShow encoded

      annotateShow encoded

      Right pathData === decoded

fdoTests :: TestTree
fdoTests =
  testGroup
    "FDO"
    [ serializeRoundtripSpecsFdo,
      serializeRoundtripPropFdo
    ]

serializeRoundtripSpecsFdo :: TestTree
serializeRoundtripSpecsFdo = testCase "decode . encode ~ id (specs)" $ do
  ts <- Timestamp.fromText "1858-11-17T00:00:00"

  let (pd1, encoded1) = mkPd ts "\NUL" "\t"
  "[Trash Info]\nPath=\t\nDeletionDate=1858-11-17T00:00:00\n" @=? encoded1
  Right pd1 @=? decode (MkPathI "\NUL") encoded1

  let (pd2, encoded2) = mkPd ts "\NUL" "\n"
  "[Trash Info]\nPath=\n\nDeletionDate=1858-11-17T00:00:00\n" @=? encoded2
  Right pd2 @=? decode (MkPathI "\NUL") encoded2
  where
    mkPd ts fileName' originalPath' =
      let pd =
            Fdo.UnsafePathData
              { fileName = MkPathI fileName',
                originalPath = MkPathI originalPath',
                created = ts
              }
       in (pd, encode pd)

serializeRoundtripPropFdo :: TestTree
serializeRoundtripPropFdo =
  testPropertyNamed "decode . encode ~ id (prop)" "serializeRoundtripPropFdo" $ do
    property $ do
      pathData@(Fdo.UnsafePathData fileName _ _) <- forAll genFdoPathData
      -- NOTE:
      -- Two caveats on injectivity:
      --
      -- 1. The fileName is thrown away. However this is intentional as we
      -- want it in the actual file name only (otherwise it is redundant).
      --
      -- We can thus think of the actual injection in terms of the implicit
      -- file name that is created by the encoding.
      --
      -- 2. We lose precision in the created timestamp, as we encode to second
      -- precision whereas the timestamp field technically has picosecond
      -- resolution. This is acceptable, as we do not care about
      -- precision > second.
      let encoded = encode pathData
          decoded = decode fileName encoded

      annotateShow encoded

      annotateShow encoded

      Right pathData === decoded

genDefaultPathData :: Gen Default.PathData
genDefaultPathData =
  Default.UnsafePathData
    <$> genPathType
    <*> genFileName
    <*> genOriginalPath
    <*> genSize
    <*> genTimestamp
  where
    genFileName = MkPathI <$> Gen.string (Range.exponential 1 100) Gen.unicode
    genOriginalPath = MkPathI <$> Gen.string (Range.linear 1 100) Gen.unicode
    genPathType = Gen.element [PathTypeDirectory, PathTypeFile]
    genSize = MkBytes <$> Gen.integral (Range.exponential 0 1_000_000_000_000)

genFdoPathData :: Gen Fdo.PathData
genFdoPathData =
  Fdo.UnsafePathData
    <$> genFileName
    <*> genOriginalPath
    <*> genTimestamp
  where
    genFileName = MkPathI <$> Gen.string (Range.exponential 1 100) Gen.unicode
    genOriginalPath = MkPathI <$> Gen.string (Range.linear 1 100) Gen.unicode

genTimestamp :: Gen Timestamp
genTimestamp = MkTimestamp <$> genLocalTime
  where
    genLocalTime =
      LocalTime
        <$> genDay
        <*> genTimeOfDay
    -- 100_000 refers to 2132, probably future-proof enough :-)
    genDay = toEnum <$> Gen.integral (Range.linear 0 100_000)
    genTimeOfDay =
      TimeOfDay
        <$> Gen.integral (Range.linear 0 23)
        <*> Gen.integral (Range.linear 0 59)
        <*> (MkFixed <$> genSec)

    -- The roundabout way of generating picoseconds is due to needing a whole
    -- number of seconds; fractions of a second will cause the tests to fail
    -- due to the lack of > second precision, mentioned above
    genSec = (\x -> x * (10 ^ picoExp)) <$> Gen.integral (Range.linear 0 59)

    picoExp :: Integer
    picoExp = 12
