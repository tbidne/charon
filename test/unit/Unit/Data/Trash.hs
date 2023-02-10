{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Trash
module Unit.Data.Trash
  ( tests,
  )
where

import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLEnc
import Effects.FileSystem.PathSize (PathSizeResult (..))
import PathSize (SubPathData (MkSubPathData))
import PathSize.Data.PathData qualified as PathSize.PathData
import SafeRm.Data.Paths (PathI, PathIndex (..))
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Exception (RootE)
import SafeRm.Trash qualified as Trash
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Data.Trash"
    [ mvTrash,
      mvTrashRootError
    ]

-- NOTE: Real IO because of MonadThrow, etc. Would be nice to mock these and
-- remove IO.
newtype PathDataT a = MkPathDataT (ReaderT (IORef Text) IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
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

instance MonadPathReader PathDataT where
  canonicalizePath = pure . ("/home/" </>)

  doesPathExist p
    | p `L.elem` exists = pure True
    | p `L.elem` nexists = pure False
    | otherwise = error p
    where
      exists =
        [ -- Need this so that our root test works i.e.
          -- test/unit/.trash/paths </> (takeFileName "/" -> "")
          -- == test/unit/.trash/paths
          "test/unit/.trash/paths"
        ]
      nexists =
        [ "test/unit/.trash/paths/foo",
          -- Due to above root test, needs a unique name
          "test/unit/.trash/paths (1)"
        ]

  doesFileExist p
    | p `L.elem` exists = pure True
    | p `L.elem` nexists = pure False
    | otherwise = error p
    where
      exists = ["/home/path/to/foo", "/"]
      nexists = []

instance MonadPathSize PathDataT where
  findLargestPaths _ p = pure (PathSizeSuccess spd)
    where
      spd = MkSubPathData $ NESeq.fromList (pd :| [])
      pd =
        PathSize.PathData.MkPathData
          { PathSize.PathData.path = p,
            PathSize.PathData.size = 0,
            PathSize.PathData.numFiles = 0,
            PathSize.PathData.numDirectories = 0
          }

-- No real IO!!!
instance MonadFileWriter PathDataT where
  writeBinaryFile _ _ = pure ()

-- No real IO!!!
instance MonadTerminal PathDataT where
  putStr = MkPathDataT . putStr

mvTrash :: TestTree
mvTrash = goldenVsStringDiff desc diff gpath $ do
  (result, _) <- runPathDataT (Trash.mvOriginalToTrash trashHome ts "path/to/foo")
  pure $ toBS result
  where
    desc = "mvOriginalToTrash success"
    gpath = goldenPath </> "move-trash.golden"

mvTrashRootError :: TestTree
mvTrashRootError = goldenVsStringDiff desc diff gpath $ do
  eformatted <-
    tryCS @_ @RootE $
      runPathDataT (Trash.mvOriginalToTrash trashHome ts "/")
  case eformatted of
    Right result ->
      pure $ "Expected exception, received result: " <> strToBS (show result)
    Left ex -> pure $ strToBS (displayException ex)
  where
    desc = "mvOriginalToTrash throws exception for root original path"
    gpath = goldenPath </> "move-trash-root-error.golden"

trashHome :: PathI TrashHome
trashHome = "test/unit/.trash"

ts :: Timestamp
ts = case fromText "2020-05-31 12:00:00" of
  Nothing -> error "[Unit.Data.PathData.ts]: Error creating timestamp"
  Just t -> t

toBS :: Text -> BSL.ByteString
toBS = TLEnc.encodeUtf8 . TL.fromStrict

strToBS :: String -> BSL.ByteString
strToBS = toBS . T.pack

goldenPath :: FilePath
goldenPath = "test/unit/Unit/Data/Trash"
