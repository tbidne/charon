{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as L
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
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

testFormatMultiline1 :: TestTree
testFormatMultiline1 = goldenVsStringDiff desc diff gpath $ do
  idx <- mkIndex
  formatted <- runConfigIO (Index.formatIndex FormatMultiline Name False idx) 100
  pure $ toBS formatted
  where
    desc = "Multiline, name, asc"
    gpath = goldenPath </> "multi-name-asc.golden"

testFormatMultiline2 :: TestTree
testFormatMultiline2 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex FormatMultiline Name True idx) 61
    pure $ toBS formatted
  where
    desc = "Multiline, name, desc"
    gpath = goldenPath </> "multi-name-desc.golden"

testFormatMultiline3 :: TestTree
testFormatMultiline3 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex FormatMultiline Size False idx) 61
    pure $ toBS formatted
  where
    desc = "Multiline, size, asc"
    gpath = goldenPath </> "multi-size-asc.golden"

testFormatMultiline4 :: TestTree
testFormatMultiline4 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex FormatMultiline Size True idx) 61
    pure $ toBS formatted
  where
    desc = "Multiline, size, desc"
    gpath = goldenPath </> "multi-size-desc.golden"

tabularFixedTests :: TestTree
tabularFixedTests =
  testGroup
    "Tabular fixed"
    [ testFormatTabularFixed1,
      testFormatTabularFixed2,
      testFormatTabularFixed3,
      testFormatTabularFixed4
    ]

testFormatTabularFixed1 :: TestTree
testFormatTabularFixed1 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex fixedTabularFormat Name False idx) 61
    pure $ toBS formatted
  where
    desc = "Tabular, name, asc"
    gpath = goldenPath </> "tabular-name-asc.golden"

testFormatTabularFixed2 :: TestTree
testFormatTabularFixed2 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex fixedTabularFormat Name True idx) 61
    pure $ toBS formatted
  where
    desc = "Tabular, name, desc"
    gpath = goldenPath </> "tabular-name-desc.golden"

testFormatTabularFixed3 :: TestTree
testFormatTabularFixed3 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex fixedTabularFormat Size False idx) 61
    pure $ toBS formatted
  where
    desc = "Tabular, size, asc"
    gpath = goldenPath </> "tabular-size-asc.golden"

testFormatTabularFixed4 :: TestTree
testFormatTabularFixed4 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- runConfigIO (Index.formatIndex fixedTabularFormat Size True idx) 61
    pure $ toBS formatted
  where
    desc = "Tabular, size, desc"
    gpath = goldenPath </> "tabular-size-desc.golden"

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
testFormatTabularAutoNormal =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing Nothing) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Auto tabular format"
    gpath = goldenPath </> "auto-normal.golden"

testFormatTabularAutoMinTermSize :: TestTree
testFormatTabularAutoMinTermSize =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing Nothing) Name False idx)
        59
    pure $ toBS formatted
  where
    desc = "Auto tabular formats minimum terminal size"
    gpath = goldenPath </> "auto-min.golden"

testFormatTabularAutoApprox :: TestTree
testFormatTabularAutoApprox =
  goldenVsStringDiff desc diff gpath $ do
    ts <- fromText "2020-05-31 12:00:00"
    let idx =
          MkIndex
            [ UnsafePathData PathTypeFile "foo" (MkPathI $ L.replicate 80 'f') (afromInteger 10) ts,
              UnsafePathData PathTypeFile (MkPathI $ L.replicate 50 'b') "bar" (afromInteger 10) ts
            ]
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing Nothing) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Auto tabular falls back to estimates for large paths"
    gpath = goldenPath </> "auto-large-approx.golden"

testFormatTabularAutoEmpty :: TestTree
testFormatTabularAutoEmpty =
  goldenVsStringDiff desc diff gpath $ do
    let idx = MkIndex []
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing Nothing) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Auto tabular empty"
    gpath = goldenPath </> "auto-empty.golden"

formatTabularAutoFail :: TestTree
formatTabularAutoFail = testCase desc $ do
  let idx = MkIndex []
  eformatted <-
    tryAnyCS $
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing Nothing) Name False idx)
        58
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
        [ "Control.Exception.Safe.throwString called with:\n\nTerminal width (58)",
          " is less than minimum width (59) for automatic tabular display. ",
          "Perhaps try multiline."
        ]

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
testFormatTabularMaxNameAutoOrig =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular (Just ColFormatMax) Nothing) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Tabular max file name, auto orig"
    gpath = goldenPath </> "tabular-max-name-auto-orig.golden"

testFormatTabularMaxNameAutoOrigTrunc :: TestTree
testFormatTabularMaxNameAutoOrigTrunc =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular (Just ColFormatMax) Nothing) Name False idx)
        70
    pure $ toBS formatted
  where
    desc = "Tabular max file name, auto orig truncates orig"
    gpath = goldenPath </> "tabular-max-name-auto-orig-trunc.golden"

testFormatTabularAutoNameMaxOrig :: TestTree
testFormatTabularAutoNameMaxOrig =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing (Just ColFormatMax)) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Tabular auto name, mad original path"
    gpath = goldenPath </> "tabular-auto-name-max-orig.golden"

testFormatTabularAutoNameMaxOrigTrunc :: TestTree
testFormatTabularAutoNameMaxOrigTrunc =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular Nothing (Just ColFormatMax)) Name False idx)
        70
    pure $ toBS formatted
  where
    desc = "Tabular auto name, max original path truncates name"
    gpath = goldenPath </> "tabular-auto-name-max-orig.golden"

testFormatTabularMaxNameMaxOrig :: TestTree
testFormatTabularMaxNameMaxOrig =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular (Just ColFormatMax) (Just ColFormatMax)) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Tabular max file name and max original path"
    gpath = goldenPath </> "tabular-max-name-max-orig.golden"

tabularMiscTests :: TestTree
tabularMiscTests =
  testGroup
    "Tabular misc"
    [ testFormatTabularFixedNameMaxOrig,
      testFormatTabularMaxNameFixedOrig
    ]

testFormatTabularFixedNameMaxOrig :: TestTree
testFormatTabularFixedNameMaxOrig =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular (Just $ ColFormatFixed 50) (Just ColFormatMax)) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Tabular fixed file name and max original path"
    gpath = goldenPath </> "tabular-fix-name-max-orig.golden"

testFormatTabularMaxNameFixedOrig :: TestTree
testFormatTabularMaxNameFixedOrig =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex (FormatTabular (Just ColFormatMax) (Just $ ColFormatFixed 50)) Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Tabular max file name and fixed original path"
    gpath = goldenPath </> "tabular-max-name-fix-orig.golden"

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

toBS :: Text -> BSL.ByteString
toBS = TLEnc.encodeUtf8 . TL.fromStrict

goldenPath :: FilePath
goldenPath = "test/unit/Unit/Data/Index"
