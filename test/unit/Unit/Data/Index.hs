{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Index
module Unit.Data.Index
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import Effects.System.Terminal
  ( MonadTerminal (getTerminalSize),
    Window (Window, height, width),
  )
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.Index (Index (MkIndex), Sort (Name, Size))
import SafeRm.Data.Index qualified as Index
import SafeRm.Data.PathData (PathDataFormat (..))
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
    [ format1,
      format2,
      format3,
      format4,
      format5,
      format6,
      format7,
      format8,
      formatAuto,
      formatAutoMin,
      formatAutoLargeApprox,
      formatAutoEmpty,
      formatAutoFail
    ]

format1 :: TestTree
format1 = testCase "Multiline, name, asc" $ do
  idx <- mkIndex
  formatted <- T.lines <$> Index.formatIndex FormatMultiline Name False idx
  assertMatches expected formatted
  where
    expected =
      Exact
        <$> [ "Type:     File",
              "Name:     bazzz",
              "Original: /path/bar/bazzz",
              "Size:     5.00K",
              "Created:  2020-05-31 12:00:00",
              "",
              "Type:     Directory",
              "Name:     d",
              "Original: /d",
              "Size:     5000.00Y",
              "Created:  2020-05-31 12:00:00",
              "",
              "Type:     Directory",
              "Name:     dir",
              "Original: /some/really/really/long/dir",
              "Size:     20.23K",
              "Created:  2020-05-31 12:00:00",
              "",
              "Type:     File",
              "Name:     f",
              "Original: /foo/path/f",
              "Size:     13.07M",
              "Created:  2020-05-31 12:00:00",
              "",
              "Type:     File",
              "Name:     foo",
              "Original: /path/foo",
              "Size:     70.00B",
              "Created:  2020-05-31 12:00:00",
              "",
              "Type:     File",
              "Name:     z",
              "Original: /z",
              "Size:     200.12K",
              "Created:  2020-05-31 12:00:00"
            ]

format2 :: TestTree
format2 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex FormatMultiline Name True idx
    pure $ toBS formatted
  where
    desc = "Multiline, name, desc"
    gpath = goldenPath </> "multi-name-desc.golden"

format3 :: TestTree
format3 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex FormatMultiline Size False idx
    pure $ toBS formatted
  where
    desc = "Multiline, size, asc"
    gpath = goldenPath </> "multi-size-asc.golden"

format4 :: TestTree
format4 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex FormatMultiline Size True idx
    pure $ toBS formatted
  where
    desc = "Multiline, size, desc"
    gpath = goldenPath </> "multi-size-desc.golden"

format5 :: TestTree
format5 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex (FormatTabular 10 22) Name False idx
    pure $ toBS formatted
  where
    desc = "Tabular, name, asc"
    gpath = goldenPath </> "tabular-name-asc.golden"

format6 :: TestTree
format6 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex (FormatTabular 10 22) Name True idx
    pure $ toBS formatted
  where
    desc = "Tabular, name, desc"
    gpath = goldenPath </> "tabular-name-desc.golden"

format7 :: TestTree
format7 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex (FormatTabular 10 22) Size False idx
    pure $ toBS formatted
  where
    desc = "Tabular, size, asc"
    gpath = goldenPath </> "tabular-size-asc.golden"

format8 :: TestTree
format8 =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <- Index.formatIndex (FormatTabular 10 22) Size True idx
    pure $ toBS formatted
  where
    desc = "Tabular, size, desc"
    gpath = goldenPath </> "tabular-size-desc.golden"

newtype ConfigIO a = MkConfigIO (ReaderT Natural IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
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

formatAuto :: TestTree
formatAuto =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex FormatTabularAuto Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Auto tabular format"
    gpath = goldenPath </> "auto-normal.golden"

formatAutoMin :: TestTree
formatAutoMin =
  goldenVsStringDiff desc diff gpath $ do
    idx <- mkIndex
    formatted <-
      runConfigIO
        (Index.formatIndex FormatTabularAuto Name False idx)
        59
    pure $ toBS formatted
  where
    desc = "Auto tabular formats minimum terminal size"
    gpath = goldenPath </> "auto-min.golden"

formatAutoLargeApprox :: TestTree
formatAutoLargeApprox =
  goldenVsStringDiff desc diff gpath $ do
    ts <- fromText "2020-05-31 12:00:00"
    let idx =
          MkIndex
            [ UnsafePathData PathTypeFile "foo" (MkPathI $ L.replicate 80 'f') (afromInteger 10) ts,
              UnsafePathData PathTypeFile (MkPathI $ L.replicate 50 'b') "bar" (afromInteger 10) ts
            ]
    formatted <-
      runConfigIO
        (Index.formatIndex FormatTabularAuto Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Auto tabular falls back to estimates for large paths"
    gpath = goldenPath </> "auto-large-approx.golden"

formatAutoEmpty :: TestTree
formatAutoEmpty =
  goldenVsStringDiff desc diff gpath $ do
    let idx = MkIndex []
    formatted <-
      runConfigIO
        (Index.formatIndex FormatTabularAuto Name False idx)
        100
    pure $ toBS formatted
  where
    desc = "Auto tabular empty"
    gpath = goldenPath </> "auto-empty.golden"

formatAutoFail :: TestTree
formatAutoFail = testCase desc $ do
  let idx = MkIndex []
  eformatted <-
    tryAnyCS $
      runConfigIO
        (Index.formatIndex FormatTabularAuto Name False idx)
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
