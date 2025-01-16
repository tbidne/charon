{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.Backend.Json.PathData
  ( tests,
  )
where

import Charon.Backend.Json.PathData (PathData (UnsafePathData))
import Charon.Backend.Json.PathData qualified as Json
import Charon.Class.Serial (Serial (decode), encodeThrowM)
import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.Paths (PathI (MkPathI))
import Charon.Data.Timestamp (Timestamp (MkTimestamp))
import Charon.Data.Timestamp qualified as Timestamp
import Data.Fixed (Fixed (MkFixed))
import Data.Text.Lazy qualified as TL
import Data.Time (LocalTime (LocalTime), TimeOfDay (TimeOfDay))
import FileSystem.OsPath (unsafeEncodeValid)
import GHC.Real ((^))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Algebra (AMonoid (zero))
import Test.Utils qualified as TestUtils
import Text.Pretty.Simple qualified as Pretty
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Backend.Json.PathData"
    ( serializeRoundtripProp
        : [serializeRoundtripSpecs specs]
    )
  where
    specs =
      [ ([osp|1|], [osp|\t|]),
        ([osp|2|], [osp|path\\nnew\\n\\nlines|]),
        ([osp|3|], [osp|path\\n\\n|])
      ]

serializeRoundtripSpecs :: [(OsPath, OsPath)] -> TestTree
serializeRoundtripSpecs params = testCase desc $ do
  ts <- Timestamp.fromText "1858-11-17T00:00:00"

  for_ params $ \(fileName, originalPath) -> do
    let pd = mkPd fileName originalPath ts
    encoded <- encodeThrowM pd

    case decode (MkPathI fileName) encoded of
      Left err ->
        assertFailure
          $ mconcat
            [ "PathData:\n\n",
              TL.unpack (Pretty.pShow pd),
              "\n\nEncoded:\n\n",
              bsToStrLenient encoded,
              "\n\nError: ",
              err
            ]
      Right decoded -> pd @=? decoded
  where
    desc = "decode . encode ~ id (specs)"

    mkPd name opath ts =
      UnsafePathData
        { pathType = MkPathTypeW PathTypeFile,
          fileName = MkPathI name,
          originalPath = MkPathI opath,
          created = ts,
          size = zero
        }

serializeRoundtripProp :: TestTree
serializeRoundtripProp =
  testPropertyNamed "decode . encode ~ id (prop)" "serializeRoundtripProp" $ do
    property $ do
      pathData <- forAll genPathData
      let fileName = pathData ^. #fileName
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
      encoded <- encodeThrowM pathData
      let decoded = decode fileName encoded

      annotateShow encoded
      annotateShow decoded

      Right pathData === decoded

genPathData :: Gen PathData
genPathData =
  UnsafePathData
    <$> genPathType
    <*> genFileName
    <*> genOriginalPath
    <*> genTimestamp
    <*> genSize
  where
    genPathType =
      MkPathTypeW
        <$> Gen.element
          [ PathTypeFile,
            PathTypeDirectory,
            PathTypeSymbolicLink
          ]
    genFileName = toPathI <$> Gen.string (Range.exponential 1 100) genPathChar
    genOriginalPath = toPathI <$> Gen.string (Range.linear 1 100) genPathChar
    genSize = MkBytes <$> Gen.integral (Range.exponential 1 1_000_000)

toPathI :: FilePath -> PathI i
toPathI = MkPathI . unsafeEncodeValid

genPathChar :: Gen Char
genPathChar = TestUtils.genPathChar False

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
