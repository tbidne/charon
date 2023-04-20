{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Data.List qualified as L
import Data.Text.Encoding qualified as TEnc
import Effects.System.Terminal
  ( MonadTerminal (getTerminalSize),
    Window (Window, height, width),
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.Index (Index (MkIndex), Sort (Name, Size))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (ColFormat (..), PathDataFormat (..))
import SafeRm.Data.PathData.Internal (PathData (UnsafePathData))
import SafeRm.Data.PathType (PathType (PathTypeDirectory, PathTypeFile))
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.Index"
    [ formattingTests
    ]

formattingTests :: TestTree
formattingTests =
  testGroup
    "Formatting"
    [ multilineTests,
      tabularFixedTests,
      tabularAutoTests,
      tabularMaxTests,
      tabularMiscTests
    ]

multilineTests :: TestTree
multilineTests =
  testGroup
    "Multiline"
    [ testFormatMultiline1,
      testFormatMultiline2,
      testFormatMultiline3,
      testFormatMultiline4
    ]

-- Tests w/ fixed format lengths, basically verifying the other args like
-- the multiline tests.
tabularFixedTests :: TestTree
tabularFixedTests =
  testGroup
    "Tabular fixed"
    [ testFormatTabularFixed1,
      testFormatTabularFixed2,
      testFormatTabularFixed3,
      testFormatTabularFixed4
    ]

testFormatMultiline1 :: TestTree
testFormatMultiline1 = testGoldenFormatParams "Multiline, name, asc" "multi-name-asc" FormatMultiline Name False

testFormatMultiline2 :: TestTree
testFormatMultiline2 = testGoldenFormatParams "Multiline, name, desc" "multi-name-desc" FormatMultiline Name True

testFormatMultiline3 :: TestTree
testFormatMultiline3 = testGoldenFormatParams "Multiline, size, asc" "multi-size-asc" FormatMultiline Size False

testFormatMultiline4 :: TestTree
testFormatMultiline4 = testGoldenFormatParams "Multiline, size, desc" "multi-size-desc" FormatMultiline Size True

testFormatTabularFixed1 :: TestTree
testFormatTabularFixed1 = testGoldenFormatParams "Tabular, name, asc" "tabular-name-asc" fixedTabularFormat Name False

testFormatTabularFixed2 :: TestTree
testFormatTabularFixed2 = testGoldenFormatParams "Tabular, name, desc" "tabular-name-desc" fixedTabularFormat Name True

testFormatTabularFixed3 :: TestTree
testFormatTabularFixed3 = testGoldenFormatParams "Tabular, size, asc" "tabular-size-asc" fixedTabularFormat Size False

testFormatTabularFixed4 :: TestTree
testFormatTabularFixed4 = testGoldenFormatParams "Tabular, size, desc" "tabular-size-desc" fixedTabularFormat Size True

fixedTabularFormat :: PathDataFormat
fixedTabularFormat = FormatTabular (Just $ ColFormatFixed 10) (Just $ ColFormatFixed 22)

newtype ConfigIO a = MkConfigIO (ReaderT Natural IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadIO,
      MonadReader Natural,
      MonadThrow
    )
    via ReaderT Natural IO

runConfigIO :: ConfigIO a -> Natural -> IO a
runConfigIO (MkConfigIO x) = runReaderT x

instance MonadTerminal ConfigIO where
  getTerminalSize =
    ask >>= \n ->
      pure $
        Window
          { height = 50,
            width = n
          }
  putStr = liftIO . putStr

instance MonadLogger ConfigIO where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNS ConfigIO where
  getNamespace = pure ""
  localNamespace _ m = m

-- Tests tabular automatic formatting i.e. nothing specified
tabularAutoTests :: TestTree
tabularAutoTests =
  testGroup
    "Tabular auto"
    [ testFormatTabularAutoNormal,
      testFormatTabularAutoMinTermSize,
      testFormatTabularAutoApprox,
      testFormatTabularAutoEmpty,
      formatTabularAutoFail
    ]

testFormatTabularAutoNormal :: TestTree
testFormatTabularAutoNormal = testGoldenFormat desc fileName mkIndex formatTabularAuto 100
  where
    desc = "Auto tabular format"
    fileName = "tabular-auto-normal"

testFormatTabularAutoMinTermSize :: TestTree
testFormatTabularAutoMinTermSize = testGoldenFormat desc fileName mkIndex formatTabularAuto 59
  where
    desc = "Auto tabular formats minimum terminal size"
    fileName = "tabular-auto-min"

testFormatTabularAutoApprox :: TestTree
testFormatTabularAutoApprox = testGoldenFormat desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular falls back to estimates for large paths"
    fileName = "tabular-auto-large-approx"
    mkIdx = do
      ts <- fromText "2020-05-31 12:00:00"
      pure $
        MkIndex
          [ UnsafePathData PathTypeFile "foo" (MkPathI $ L.replicate 80 'f') (afromInteger 10) ts,
            UnsafePathData PathTypeFile (MkPathI $ L.replicate 50 'b') "bar" (afromInteger 10) ts
          ]

testFormatTabularAutoEmpty :: TestTree
testFormatTabularAutoEmpty = testGoldenFormat desc fileName mkIdx formatTabularAuto 100
  where
    desc = "Auto tabular empty"
    fileName = "tabular-auto-empty"
    mkIdx = pure $ MkIndex []

formatTabularAutoFail :: TestTree
formatTabularAutoFail = testCase desc $ do
  let idx = MkIndex []
  eformatted <-
    tryAnyCS $
      runConfigIO
        (Index.formatIndex formatTabularAuto Name False idx)
        53
  case eformatted of
    Right result ->
      assertFailure $
        "Expected exception, received result: " <> show result
    Left ex ->
      assertBool (displayException ex) (expected `L.isPrefixOf` displayException ex)
  where
    desc = "Auto tabular throws error for small terminal width"
    expected =
      mconcat
        [ "Control.Exception.Safe.throwString called with:\n\nTerminal width (53)",
          " is less than minimum width (54) for automatic tabular display. ",
          "Perhaps try multiline."
        ]

-- Tests tabular w/ max options
tabularMaxTests :: TestTree
tabularMaxTests =
  testGroup
    "Tabular max"
    [ testFormatTabularMaxNameAutoOrig,
      testFormatTabularMaxNameAutoOrigTrunc,
      testFormatTabularAutoNameMaxOrig,
      testFormatTabularAutoNameMaxOrigTrunc,
      testFormatTabularMaxNameMaxOrig
    ]

testFormatTabularMaxNameAutoOrig :: TestTree
testFormatTabularMaxNameAutoOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig"
    fileName = "tabular-max-name-auto-orig"

testFormatTabularMaxNameAutoOrigTrunc :: TestTree
testFormatTabularMaxNameAutoOrigTrunc = testGoldenFormat desc fileName mkIndex fmt 70
  where
    fmt = FormatTabular (Just ColFormatMax) Nothing
    desc = "Tabular max file name, auto orig truncates orig"
    fileName = "tabular-max-name-auto-orig-trunc"

testFormatTabularAutoNameMaxOrig :: TestTree
testFormatTabularAutoNameMaxOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path"
    fileName = "tabular-auto-name-max-orig"

testFormatTabularAutoNameMaxOrigTrunc :: TestTree
testFormatTabularAutoNameMaxOrigTrunc = testGoldenFormat desc fileName mkIndex fmt 70
  where
    fmt = FormatTabular Nothing (Just ColFormatMax)
    desc = "Tabular auto name, max original path truncates name"
    fileName = "tabular-auto-name-max-orig-trunc"

testFormatTabularMaxNameMaxOrig :: TestTree
testFormatTabularMaxNameMaxOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular (Just ColFormatMax) (Just ColFormatMax)
    desc = "Tabular max file name and max original path"
    fileName = "tabular-max-name-max-orig"

-- Misc tabular tests e.g. interactions between various options
tabularMiscTests :: TestTree
tabularMiscTests =
  testGroup
    "Tabular misc"
    [ testFormatTabularFixedNameMaxOrig,
      testFormatTabularFixedNameAutoOrig,
      testFormatTabularMaxNameFixedOrig,
      testFormatTabularAutoNameFixedOrig
    ]

testFormatTabularFixedNameMaxOrig :: TestTree
testFormatTabularFixedNameMaxOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular (Just $ ColFormatFixed 50) (Just ColFormatMax)
    desc = "Tabular fixed file name and max original path"
    fileName = "tabular-fix-name-max-orig"

testFormatTabularFixedNameAutoOrig :: TestTree
testFormatTabularFixedNameAutoOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular (Just $ ColFormatFixed 50) Nothing
    desc = "Tabular fixed file name and auto original path"
    fileName = "tabular-fix-name-auto-orig"

testFormatTabularMaxNameFixedOrig :: TestTree
testFormatTabularMaxNameFixedOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular (Just ColFormatMax) (Just $ ColFormatFixed 50)
    desc = "Tabular max file name and fixed original path"
    fileName = "tabular-max-name-fix-orig"

testFormatTabularAutoNameFixedOrig :: TestTree
testFormatTabularAutoNameFixedOrig = testGoldenFormat desc fileName mkIndex fmt 100
  where
    fmt = FormatTabular Nothing (Just $ ColFormatFixed 50)
    desc = "Tabular auto file name and fixed original path"
    fileName = "tabular-auto-name-fix-orig"

mkIndex :: (MonadFail f) => f Index
mkIndex = do
  ts <- ts'
  pure $
    MkIndex
      [ UnsafePathData PathTypeFile "foo" "/path/foo" (afromInteger 70) ts,
        UnsafePathData PathTypeFile "bazzz" "/path/bar/bazzz" (afromInteger 5_000) ts,
        UnsafePathData PathTypeDirectory "dir" "/some/really/really/long/dir" (afromInteger 20_230) ts,
        UnsafePathData PathTypeFile "f" "/foo/path/f" (afromInteger 13_070_000) ts,
        UnsafePathData PathTypeDirectory "d" "/d" largeFile ts,
        UnsafePathData PathTypeFile "z" "/z" (afromInteger 200_120) ts
      ]
  where
    -- 5,000 Y
    largeFile = afromInteger 5_000_000_000_000_000_000_000_000_000
    ts' :: (MonadFail f) => f Timestamp
    ts' = fromText "2020-05-31 12:00:00"

toBS :: Text -> ByteString
toBS = TEnc.encodeUtf8

formatTabularAuto :: PathDataFormat
formatTabularAuto = FormatTabular Nothing Nothing

-- | Golden tests for different combinations of PathDataFormat + Index +
-- Sort + Reverse.
testGoldenFormatParams ::
  -- | Test description
  String ->
  -- | Golden filepath
  FilePath ->
  -- | Style
  PathDataFormat ->
  -- | Column upon which to sort
  Sort ->
  -- | Sort style
  Bool ->
  TestTree
testGoldenFormatParams desc fileName style sortCol rev =
  testGolden desc fileName mkIndex style sortCol rev 61

-- Golden tests for different combinations of PathDataFormat + Index +
-- Terminal size.
--
-- @since 0.1
testGoldenFormat ::
  -- | Test description
  String ->
  -- | Golden filepath
  FilePath ->
  -- | Action producing the index
  IO Index ->
  -- | Style
  PathDataFormat ->
  -- | Terminal size
  Natural ->
  TestTree
testGoldenFormat desc fileName mkIdx style =
  testGolden desc fileName mkIdx style Name False

-- where
--  (gpath, apath) = mkGoldenPaths fileName

-- | General function for running golden tests.
--
-- @since 0.1
testGolden ::
  -- | Test description
  String ->
  -- | Golden filepath
  FilePath ->
  -- | Action producing the index
  IO Index ->
  -- | Style
  PathDataFormat ->
  -- | Column on which to sort
  Sort ->
  -- | True = asc sort, false = desc
  Bool ->
  -- | Terminal size
  Natural ->
  TestTree
testGolden desc fileName mkIdx style sortFn rev termWidth = goldenVsFile desc gpath apath $ do
  idx <- mkIdx
  let fmt = Index.formatIndex style sortFn rev idx
  formatted <- runConfigIO fmt termWidth
  writeBinaryFile apath (toBS formatted)
  where
    (gpath, apath) = mkGoldenPaths fileName

mkGoldenPaths :: FilePath -> (FilePath, FilePath)
mkGoldenPaths fp = (goldenPath </> fp <> ".golden", goldenPath </> fp <> ".actual")
  where
    goldenPath = "test/unit/Unit/Data/Index"
