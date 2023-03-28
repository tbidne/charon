{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Unit tests for Data.Trash
module Unit.Data.Trash
  ( tests,
  )
where

import Data.List qualified as L
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Text qualified as T
import Effects.FileSystem.PathSize (PathSizeResult (..))
import PathSize (SubPathData (MkSubPathData))
import PathSize.Data.PathData qualified as PathSize.PathData
import SafeRm.Data.Paths (PathI, PathIndex (..), liftPathI')
import SafeRm.Data.Timestamp (Timestamp, fromText)
import SafeRm.Exception (EmptyPathE, RootE)
import SafeRm.Trash qualified as Trash
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Trash"
    [ mvTrash,
      mvTrashWhitespace,
      mvTrashRootError,
      mvTrashEmptyError
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

instance MonadLoggerNS PathDataT where
  getNamespace = pure ""
  localNamespace _ = id

-- No real IO!!!
instance MonadPathWriter PathDataT where
  renameFile p1 p2 =
    ask >>= \ref ->
      writeIORef ref ("renamed " <> T.pack p1 <> " to " <> T.pack p2)

instance MonadPathReader PathDataT where
  canonicalizePath = pure . (windowsify "/home/" </>)

  doesPathExist p
    | p `L.elem` nexists = pure False
    | otherwise = error $ "Path: '" <> p <> "'"
    where
      nexists =
        windowsify
          <$> [ "test/unit/.trash/paths/foo",
                "test/unit/.trash/paths/ "
              ]

  doesFileExist p
    | p `L.elem` exists = pure True
    | otherwise = error p
    where
      exists = windowsify <$> ["/home/path/to/foo", "/", "/home/ "]

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
mvTrash = testCase "mvOriginalToTrash success" $ do
  (result, _) <- runPathDataT (Trash.mvOriginalToTrash trashHome ts (liftPathI' windowsify "path/to/foo"))
  windowsify "renamed /home/path/to/foo to test/unit/.trash/paths/foo" @=? T.unpack result

mvTrashWhitespace :: TestTree
mvTrashWhitespace = testCase "mvOriginalToTrash whitespace success" $ do
  (result, _) <- runPathDataT (Trash.mvOriginalToTrash trashHome ts " ")
  windowsify "renamed /home/  to test/unit/.trash/paths/ " @=? T.unpack result

mvTrashRootError :: TestTree
mvTrashRootError = testCase desc $ do
  eformatted <-
    tryCS @_ @RootE $
      runPathDataT (Trash.mvOriginalToTrash trashHome ts rootDir)
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete root! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for root original path"
#if WINDOWS
    rootDir = "C:\\"
#else
    rootDir = "/"
#endif

mvTrashEmptyError :: TestTree
mvTrashEmptyError = testCase desc $ do
  eformatted <-
    tryCS @_ @EmptyPathE $
      runPathDataT (Trash.mvOriginalToTrash trashHome ts "")
  case eformatted of
    Right result ->
      assertFailure $ "Expected exception, received result: " <> show result
    Left ex -> "Attempted to delete the empty path! This is not allowed." @=? displayException ex
  where
    desc = "mvOriginalToTrash throws exception for empty original path"

trashHome :: PathI TrashHome
trashHome = liftPathI' windowsify "test/unit/.trash"

ts :: Timestamp
ts = case fromText "2020-05-31 12:00:00" of
  Nothing -> error "[Unit.Data.PathData.ts]: Error creating timestamp"
  Just t -> t

windowsify :: String -> String
#if WINDOWS
windowsify [] = []
windowsify (c:cs)
  | c == '/' = '\\' : windowsify cs
  | otherwise = c : windowsify cs
#else
windowsify = id
#endif
