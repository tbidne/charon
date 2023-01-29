{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.PathData
module Unit.Data.PathData
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import Numeric.Literal.Integer (FromInteger (afromInteger))
import SafeRm.Data.PathData
  ( PathData (MkPathData),
  )
import SafeRm.Data.PathData qualified as PathData
import SafeRm.Data.PathType (PathType (PathTypeFile))
import SafeRm.Data.Paths (PathI, PathIndex (..))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Exception (RootE)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.PathData"
    [ mvTrash,
      mvTrashRootOriginalPathError,
      mvTrashRootNamePathError
    ]

newtype PathDataT a = MkPathDataT (ReaderT (IORef Text) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadIORef,
      MonadReader (IORef Text),
      MonadThrow
    )
    via ReaderT (IORef Text) IO

runPathDataT :: PathDataT a -> IO (Text, a)
runPathDataT (MkPathDataT x) = do
  ref <- newIORef ""
  result <- runReaderT x ref
  t <- readIORef ref
  pure (t, result)

instance MonadLogger PathDataT where
  monadLoggerLog _ _ _ _ = pure ()

instance MonadLoggerNamespace PathDataT where
  getNamespace = pure ""
  localNamespace _ = id

-- No real IO!!!
instance MonadPathWriter PathDataT where
  renameFile p1 p2 =
    ask >>= \ref ->
      writeIORef ref ("renamed " <> T.pack p1 <> " to " <> T.pack p2)

mvTrash :: TestTree
mvTrash = goldenVsStringDiff desc diff gpath $ do
  (result, _) <- runPathDataT (PathData.mvOriginalToTrash trashHome pd)
  pure $ toBS result
  where
    desc = "mvOriginalToTrash success"
    pd =
      MkPathData
        { pathType = PathTypeFile,
          fileName = "foo",
          originalPath = "path/to/foo",
          size = fileSize,
          created = ts
        }
    gpath = goldenPath </> "move-trash.golden"

mvTrashRootOriginalPathError :: TestTree
mvTrashRootOriginalPathError = goldenVsStringDiff desc diff gpath $ do
  eformatted <-
    tryCS @_ @RootE $
      runPathDataT (PathData.mvOriginalToTrash trashHome pd)
  case eformatted of
    Right result ->
      pure $ "Expected exception, received result: " <> strToBS (show result)
    Left ex -> pure $ strToBS (displayException ex)
  where
    desc = "mvOriginalToTrash throws exception for root original path"
    pd =
      MkPathData
        { pathType = PathTypeFile,
          fileName = "foo",
          originalPath = "/",
          size = fileSize,
          created = ts
        }
    gpath = goldenPath </> "move-trash-root-original-error.golden"

mvTrashRootNamePathError :: TestTree
mvTrashRootNamePathError = goldenVsStringDiff desc diff gpath $ do
  eformatted <-
    tryCS @_ @RootE $
      runPathDataT (PathData.mvOriginalToTrash trashHome pd)
  case eformatted of
    Right result ->
      pure $ "Expected exception, received result: " <> strToBS (show result)
    Left ex -> pure $ strToBS (displayException ex)
  where
    desc = "mvOriginalToTrash throws exception for root name"
    pd =
      MkPathData
        { pathType = PathTypeFile,
          fileName = "/",
          originalPath = "foo",
          size = fileSize,
          created = ts
        }
    gpath = goldenPath </> "move-trash-root-name-error.golden"

trashHome :: PathI TrashHome
trashHome = "test/unit/.trash"

fileSize :: Bytes B Natural
fileSize = afromInteger 70

ts :: Timestamp
ts = case fromText "2020-05-31 12:00:00" of
  Nothing -> error "[Unit.Data.PathData.ts]: Error creating timestamp"
  Just t -> t

toBS :: Text -> BSL.ByteString
toBS = TLEnc.encodeUtf8 . TL.fromStrict

strToBS :: String -> BSL.ByteString
strToBS = toBS . T.pack

goldenPath :: FilePath
goldenPath = "test/unit/Unit/Data/PathData"
