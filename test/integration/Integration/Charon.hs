{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Property tests for the Charon API.
module Integration.Charon
  ( tests,
  )
where

import Charon qualified
import Charon.Backend.Data (Backend)
import Charon.Backend.Data qualified as Backend.Data
import Charon.Data.PathData (PathData)
import Charon.Data.PathType (PathTypeW (MkPathTypeW))
import Charon.Data.Paths (PathI (MkPathI))
import Charon.Data.UniqueSeqNE (UniqueSeqNE)
import Charon.Data.UniqueSeqNE qualified as USeqNE
import Charon.Env (HasBackend, HasTrashHome (getTrashHome))
import Charon.Env qualified as Env
import Charon.Exception (TrashEntryNotFoundE)
import Charon.Runner.CharonT (CharonT (MkCharonT))
import Charon.Runner.Env
  ( Env (MkEnv, backend, logEnv, trashHome),
    LogEnv (MkLogEnv),
  )
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Char qualified as Ch
import Data.HashSet qualified as HSet
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.PathReader (_PathTypeDirectory)
import Effects.LoggerNS (Namespace, defaultLogFormatter)
import Effects.LoggerNS qualified as Logger
import GHC.IsList (IsList (toList))
import Hedgehog (PropertyT)
import Integration.AsciiOnly (AsciiOnly (MkAsciiOnly))
import Integration.Prelude
import Integration.Utils (PathWithType (MkPathWithType))
import Integration.Utils qualified as IntUtils
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet), withGuard_)

-- Custom type for running the tests. Fo now, the only reason we do not use
-- CharonT is to override getFileSize so that expected errors in tests
-- do not spam the console logs (i.e. retrieving the size for a bad path).
-- We could use this to later verify logs, if we wished.

data IntEnv = MkIntEnv
  { coreEnv :: Env IO,
    termLogsRef :: IORef [Text],
    logsRef :: IORef [Text],
    namespace :: Namespace
  }

makeFieldLabelsNoPrefix ''IntEnv

instance HasBackend IntEnv where
  getBackend = view (#coreEnv % #backend)

instance HasTrashHome IntEnv where
  getTrashHome = view (#coreEnv % #trashHome)

-- | Type for running integration tests.
newtype IntIO a = MkIntIO (ReaderT IntEnv IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadAsync,
      MonadIO,
      MonadCatch,
      MonadFileReader,
      MonadFileWriter,
      MonadHandleWriter,
      MonadIORef,
      MonadPathReader,
      MonadPathWriter,
      MonadPosixCompat,
      MonadReader IntEnv,
      MonadThrow,
      MonadTime
    )
    via (CharonT IntEnv IO)

instance MonadLogger IntIO where
  monadLoggerLog loc _ lvl msg = do
    formatted <- Logger.formatLog (defaultLogFormatter loc) lvl msg
    let txt = Logger.logStrToText formatted
    logsRef <- asks (view #logsRef)
    modifyIORef' logsRef (txt :)

instance MonadLoggerNS IntIO where
  getNamespace = view #namespace <$> ask
  localNamespace f = local (over' #namespace f)

-- Wno-missing-methods
instance MonadTerminal IntIO where
  putStr s =
    asks (view #termLogsRef)
      >>= \ref -> modifyIORef' ref ((:) . T.pack $ s)

-- | Default way to run integration tests. Ensures uncaught exceptions do not
-- mess with annotations
usingIntIO :: IntEnv -> IntIO a -> PropertyT IO a
usingIntIO env rdr = do
  -- NOTE: Evidently annotations do not work with uncaught exceptions.
  -- Therefore we catch any exceptions here, print out any relevant data,
  -- then use Hedgehog's failure to die.
  result <-
    usingIntIONoCatch env rdr `catchAny` \ex -> do
      printLogs
      annotate $ displayException ex
      failure

  -- for help debugging CI failures
  withGuard_ "INT_LOGS" ExpectEnvSet printLogs

  pure result
  where
    printLogs = do
      termLogs <- liftIO $ readIORef (env ^. #termLogsRef)
      logs <- liftIO $ readIORef (env ^. #logsRef)
      annotate "TERMINAL LOGS"
      for_ termLogs (annotate . T.unpack)
      annotate "FILE LOGS"
      for_ (L.reverse logs) (annotate . T.unpack)

-- | Use this for when we are expecting an exception and want to specifically
-- handle it in the test
usingIntIONoCatch :: IntEnv -> IntIO a -> PropertyT IO a
usingIntIONoCatch env (MkIntIO rdr) = liftIO $ runReaderT rdr env

tests :: IO OsPath -> TestTree
tests testDir =
  testGroup
    "Charon"
    (testsBackend testDir <$> [minBound .. maxBound])

testsBackend :: IO OsPath -> Backend -> TestTree
testsBackend testDir b =
  testGroup
    (Backend.Data.backendTestDesc b)
    [ delete b testDir,
      deleteSome b testDir,
      permDelete b testDir,
      permDeleteSome b testDir,
      restore b testDir,
      restoreSome b testDir,
      emptyTrash b testDir,
      metadata b testDir
    ]

delete :: Backend -> IO OsPath -> TestTree
delete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "All paths are deleted" "delete" $ do
    property $ do
      annotateShow b
      testDir <- getTestPath mtestDir [osp|delete|] backend
      (α, fps) <- forAll (IntUtils.genFileNameSet b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      annotateShow $ show $ USeqNE.map (fmap Ch.isPrint . IntUtils.normedFpToFp) fps
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI αTestPaths)

      annotate "Assert lookup"

      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- get index
      index <- getIndex env
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      αTestPaths ^. #set === indexOrigPaths

deleteSome :: Backend -> IO OsPath -> TestTree
deleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|deleteSome|] backend
      ((α, _), (β, _)) <- forAll (IntUtils.gen2FileNameSets b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          (_, _, βTestPaths) = mkPaths testDir β

          -- αTest = toTestDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      -- should succeed on α and fail on β
      let toDelete = αTestPaths `USeqNE.union` βTestPaths

      caughtEx <-
        tryCS @_ @IOException
          $ usingIntIONoCatch env (Charon.delete (USeqNE.map MkPathI toDelete))

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- should have printed exactly 1 message for each bad filename
      termLogs <- liftIO $ readIORef (env ^. #termLogsRef)
      length termLogs === length β

      -- assert original files moved to trash
      annotate "Assert lookup"

      -- get index
      index <- getIndex env
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- index should exactly match α
      αTestPaths ^. #set === indexOrigPaths

permDelete :: Backend -> IO OsPath -> TestTree
permDelete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDelete" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|permDelete|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let trashDir = testDir </> [osp|.trash|]
          (αNames, αTest, αTestPaths) = mkPaths testDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI αTestPaths)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- permanently delete files
      let toPermDelete = USeqNE.map MkPathI αNames
      usingIntIO env $ Charon.permDelete True toPermDelete

      -- get index
      index <- getIndex env
      annotateShow index

      [] === index
      assertPathsDoNotExist αTestPaths
  where
    desc = "All trash entries are permanently deleted"

permDeleteSome :: Backend -> IO OsPath -> TestTree
permDeleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDeleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|permDeleteSome|] backend
      ((α, _), (β, _), (γ, _)) <- forAll (IntUtils.gen3FileNameSets b)
      let (αNames, αTest, αTestPaths) = mkPaths testDir α
          (βNames, _, _) = mkPaths testDir β
          (_, γTest, γTestPaths) = mkPaths testDir γ

          toDelete = αTestPaths `USeqNE.union` γTestPaths
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      setupDir testDir (αTest `USeqNE.union` γTest)

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- permanently delete files
      -- should succeed on α and fail on β
      let toPermDelete = USeqNE.map MkPathI (αNames `USeqNE.union` βNames)
      annotateShow toPermDelete

      caughtEx <-
        tryCS @_ @TrashEntryNotFoundE
          $ usingIntIONoCatch env (Charon.permDelete True toPermDelete)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- getIndex env
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      γTestPaths ^. #set === indexOrigPaths

      annotate "Assert files exist"
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: Backend -> IO OsPath -> TestTree
restore backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Restores all trash entries" "restore" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|restore|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let (αNames, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI αTestPaths)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- restore files
      let toRestore = USeqNE.map MkPathI αNames
      usingIntIO env $ Charon.restore toRestore

      -- get index
      index <- getIndex env
      annotateShow index

      [] === index
      annotate "Assert files exist"
      assertPathsExist αTestPaths

restoreSome :: Backend -> IO OsPath -> TestTree
restoreSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "restoreSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|restoreSome|] backend
      ((α, _), (β, _), (γ, _)) <- forAll (IntUtils.gen3FileNameSets b)
      let (αNames, αTest, αTestPaths) = mkPaths testDir α
          (βNames, _, _) = mkPaths testDir β
          (_, γTest, γTestPaths) = mkPaths testDir γ

          toDelete = αTestPaths `USeqNE.union` γTestPaths
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      setupDir testDir (αTest `USeqNE.union` γTest)

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- restore
      -- should succeed on α and fail on β
      let toRestore = USeqNE.map MkPathI (αNames `USeqNE.union` βNames)
      annotateShow toRestore

      caughtEx <-
        tryCS @_ @TrashEntryNotFoundE
          $ usingIntIONoCatch env (Charon.restore toRestore)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- getIndex env
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      γTestPaths ^. #set === indexOrigPaths

      annotate "Assert files exist"
      assertPathsExist αTestPaths
      annotate "Assert lookup"
  where
    desc = "Some trash entries are restored, others error"

emptyTrash :: Backend -> IO OsPath -> TestTree
emptyTrash backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Empties the trash" "empty" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|emptyTrash|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI αTestPaths)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- empty trash
      usingIntIO env $ Charon.emptyTrash True

      -- get index
      index <- getIndex env
      annotateShow index

      [] === index
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

metadata :: Backend -> IO OsPath -> TestTree
metadata backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Retrieves metadata" "metadata" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|metadata|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let (_, αTest, αTestPaths) = mkPaths testDir α
          trashDir = testDir </> [osp|.trash|]
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ Charon.delete (USeqNE.map MkPathI αTestPaths)

      -- assert original files moved to trash
      annotate "Assert lookup"
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTestPaths

      -- get metadata
      metadata' <- usingIntIO env Charon.getMetadata
      annotateShow metadata'

      length α === natToInt (metadata' ^. #numEntries)

      -- α >= numFiles because the latter can include dirs
      -- diff (length α) (>=) (natToInt $ metadata' ^. #numFiles)
      countFiles α === natToInt (metadata' ^. #numFiles)

natToInt :: (HasCallStack) => Natural -> Int
natToInt i
  | i <= intMax = fromIntegral i
  | otherwise = error $ "natToInt: Too large to convert to Int: " <> show i
  where
    intMax = fromIntegral (maxBound :: Int)

toOrigPath :: HashSet OsPath -> PathData -> HashSet OsPath
toOrigPath acc pd = HSet.insert (pd ^. #originalPath % #unPathI) acc

mkEnv :: Backend -> OsPath -> IO IntEnv
mkEnv backend fp = do
  termLogsRef <- newIORef []
  logsRef <- newIORef []

  pure
    $ MkIntEnv
      { coreEnv =
          MkEnv
            { trashHome = MkPathI fp,
              backend,
              logEnv = MkLogEnv Nothing ""
            },
        termLogsRef,
        logsRef,
        namespace = "integration"
      }

-- NOTE: [OSX temp symlink]
--
-- This exists because in the osx tests', canonicalize transforms a path
--
--    /var/...
--
-- into
--
--    /private/var/...
--
-- due to some symlink shenanigans. This causes tests to fail because the
-- trash dir we create looks like /var/... whereas the PathData's
-- originalPath is /private/var/...
--
-- Thus we canonicalize the test path as well, allowing our
-- "deleted paths match expected paths" checks to succeed.
--
-- This is not needed on linux but also appears unharmful.
getTestPath :: (MonadIO m) => IO OsPath -> OsPath -> Backend -> m OsPath
getTestPath mtestPath p backend = do
  testPath <- liftIO $ makeAbsolute =<< mtestPath
  pure $ testPath </> p </> Backend.Data.backendArgOsPath backend

setupDir ::
  ( MonadCatch m,
    MonadIO m,
    MonadTest m
  ) =>
  OsPath ->
  UniqueSeqNE PathWithType ->
  m ()
setupDir dir paths = do
  let action = do
        clearDirectory dir

        for_ paths $ \(MkPathWithType (p, MkPathTypeW ty)) -> case ty of
          PathTypeFile -> createFileContents (p, "")
          PathTypeDirectory -> createDirectory p
          PathTypeSymbolicLink -> createSymlink (F p)
        assertPathsExist (fmap (view (#unPathWithType % _1)) paths')

  action `catchAny` \ex -> do
    annotate $ displayException ex
    failure
  where
    paths' :: [PathWithType]
    paths' = toList $ view #seq paths

-- | Given a test dir and a set of names, forms the paths needed to run
-- tests.
mkPaths ::
  -- | TestDir
  OsPath ->
  -- | Set of pathNames
  UniqueSeqNE PathWithType ->
  -- | (pathNames, (testDir </> pathNames, a), testDir </> pathNames)
  ( UniqueSeqNE OsPath,
    UniqueSeqNE PathWithType,
    UniqueSeqNE OsPath
  )
mkPaths testDir paths =
  ( USeqNE.map (view (#unPathWithType % _1)) paths,
    prefixed,
    USeqNE.map (view (#unPathWithType % _1)) prefixed
  )
  where
    prefixed = USeqNE.map (\(MkPathWithType (p, t)) -> MkPathWithType (testDir </> p, t)) paths

countFiles :: UniqueSeqNE PathWithType -> Int
countFiles = length . filter isNotDir . toList . view #seq
  where
    isNotDir :: PathWithType -> Bool
    isNotDir = not . is (#unPathWithType % _2 % #unPathTypeW % _PathTypeDirectory)

getIndex :: IntEnv -> PropertyT IO (Seq PathData)
getIndex env = fmap (view _1) . view #unIndex <$> usingIntIO env Charon.getIndex
