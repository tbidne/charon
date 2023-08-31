{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Property tests for the SafeRm API.
module Integration.SafeRm
  ( tests,
  )
where

import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Bifunctor (Bifunctor (first))
import Data.Char qualified as Ch
import Data.HashSet qualified as HSet
import Data.Hashable (Hashable (hash))
import Data.List (unzip)
import Data.List qualified as L
import Data.Text qualified as T
import Effects.FileSystem.Utils qualified as FsUtils
import Effects.LoggerNS (Namespace, defaultLogFormatter)
import Effects.LoggerNS qualified as Logger
import GHC.Exts (IsList (Item, fromList))
import Hedgehog (GenT, PropertyT, forAll)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range (Range)
import Hedgehog.Range qualified as Range
import Integration.AsciiOnly (AsciiOnly (MkAsciiOnly))
import Integration.Prelude
import Integration.Utils qualified as IntUtils
import SafeRm qualified
import SafeRm.Data.Backend (Backend)
import SafeRm.Data.Backend qualified as Backend
import SafeRm.Data.PathData (PathData)
import SafeRm.Data.Paths (PathI (MkPathI))
import SafeRm.Data.UniqueSeq (UniqueSeq, fromFoldable)
import SafeRm.Data.UniqueSeq qualified as USeq
import SafeRm.Env (HasBackend, HasTrashHome (getTrashHome))
import SafeRm.Env qualified as Env
import SafeRm.Exception (FileNotFoundE, TrashEntryNotFoundE)
import SafeRm.Runner.Env
  ( Env (MkEnv, backend, logEnv, trashHome),
    LogEnv (MkLogEnv),
  )
import SafeRm.Runner.SafeRmT (SafeRmT (MkSafeRmT))
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet), withGuard_)
import System.OsPath qualified as OsPath
import Test.Utils qualified as TestUtils

-- Custom type for running the tests. Fo now, the only reason we do not use
-- SafeRmT is to override getFileSize so that expected errors in tests
-- do not spam the console logs (i.e. retrieving the size for a bad path).
-- We could use this to later verify logs, if we wished.

data IntEnv = MkIntEnv
  { coreEnv :: Env IO,
    termLogsRef :: IORef [Text],
    logsRef :: IORef [Text],
    namespace :: !Namespace
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
      MonadThread,
      MonadThrow,
      MonadTime
    )
    via (SafeRmT IntEnv IO)

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
    "SafeRm"
    (testsBackend testDir <$> [minBound .. maxBound])

testsBackend :: IO OsPath -> Backend -> TestTree
testsBackend testDir b =
  testGroup
    (Backend.backendTestDesc b)
    [ delete b testDir
    -- deleteSome b testDir,
    -- permDelete b testDir,
    -- permDeleteSome b testDir,
    -- restore b testDir,
    -- restoreSome b testDir,
    -- emptyTrash b testDir,
    -- metadata b testDir
    ]

delete :: Backend -> IO OsPath -> TestTree
delete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "All paths are deleted" "delete" $ do
    property $ do
      annotateShow b
      testDir <- getTestPath mtestDir [osp|delete|] backend
      (α, fps) <- forAll (IntUtils.genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> [osp|.trash|]
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      annotateShow $ show $ USeq.map (fmap Ch.isPrint) fps
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      αTest ^. #set === indexOrigPaths

deleteSome :: Backend -> IO OsPath -> TestTree
deleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Some paths are deleted, others error" "deleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|deleteSome|] backend
      ((α, _), (β, _)) <- forAll (IntUtils.gen2FileNameSets b)
      let toTestDir = USeq.map (testDir </>)

          αTest = toTestDir α
          trashDir = testDir </> [osp|.trash|]
          αTrash = mkTrashPaths backend trashDir α
          βTrash = mkTrashPaths backend trashDir β
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      -- should succeed on α and fail on β
      let toDelete = αTest `USeq.union` toTestDir β

      caughtEx <-
        tryCS @_ @FileNotFoundE
          $ usingIntIONoCatch env (SafeRm.delete (USeq.map MkPathI toDelete))

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
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist βTrash

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- index should exactly match α
      αTest ^. #set === indexOrigPaths

permDelete :: Backend -> IO OsPath -> TestTree
permDelete backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDelete" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|permDelete|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let trashDir = testDir </> [osp|.trash|]
          αTest = USeq.map (testDir </>) α
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- permanently delete files
      let toPermDelete = USeq.map MkPathI α
      usingIntIO env $ SafeRm.permDelete True toPermDelete

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      [] === index
      assertPathsDoNotExist (αTest `USeq.union` αTrash)
  where
    desc = "All trash entries are permanently deleted"

permDeleteSome :: Backend -> IO OsPath -> TestTree
permDeleteSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "permDeleteSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|permDeleteSome|] backend
      ((α, _), (β, _), (γ, _)) <- forAll (IntUtils.gen3FileNameSets b)
      let toTestDir = USeq.map (testDir </>)

          toDelete = toTestDir (α `USeq.union` γ)
          trashDir = testDir </> [osp|.trash|]
          trashSet = mkTrashPaths backend trashDir (α `USeq.union` γ)
          αTrash = mkTrashPaths backend trashDir α
          βTrash = mkTrashPaths backend trashDir β
          γTrash = mkTrashPaths backend trashDir γ
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      setupDir testDir toDelete

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist trashSet
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- permanently delete files
      -- should succeed on α and fail on β
      let toPermDelete = USeq.map MkPathI (α `USeq.union` β)
      annotateShow toPermDelete

      caughtEx <-
        tryCS @_ @TrashEntryNotFoundE
          $ usingIntIONoCatch env (SafeRm.permDelete True toPermDelete)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      toTestDir γ ^. #set === indexOrigPaths

      annotate "Assert files do not exist"
      assertPathsDoNotExist (αTrash `USeq.union` βTrash)
      annotate "Assert files exist"
      assertPathsExist γTrash
  where
    desc = "Some trash entries are permanently deleted, others error"

restore :: Backend -> IO OsPath -> TestTree
restore backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Restores all trash entries" "restore" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|restore|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> [osp|.trash|]
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- restore files
      let toRestore = USeq.map MkPathI α
      usingIntIO env $ SafeRm.restore toRestore

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      [] === index
      annotate "Assert files exist"
      assertPathsExist αTest
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTrash

restoreSome :: Backend -> IO OsPath -> TestTree
restoreSome backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed desc "restoreSome" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|restoreSome|] backend
      ((α, _), (β, _), (γ, _)) <- forAll (IntUtils.gen3FileNameSets b)
      let toTestDir = USeq.map (testDir </>)

          toDelete = toTestDir (α `USeq.union` γ)
          trashDir = testDir </> [osp|.trash|]
          αTrash = mkTrashPaths backend trashDir α
          γTrash = mkTrashPaths backend trashDir γ
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow toDelete

      -- create files and assert existence
      setupDir testDir toDelete

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI toDelete)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist toDelete

      -- restore
      -- should succeed on α and fail on β
      let toRestore = USeq.map MkPathI (α `USeq.union` β)
      annotateShow toRestore

      caughtEx <-
        tryCS @_ @TrashEntryNotFoundE
          $ usingIntIONoCatch env (SafeRm.restore toRestore)

      ex <-
        either
          pure
          (\_ -> annotate "Expected exceptions, received none" *> failure)
          caughtEx

      annotateShow ex

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index
      let indexOrigPaths = foldl' toOrigPath HSet.empty index

      -- γ should still exist in the trash index
      toTestDir γ ^. #set === indexOrigPaths

      annotate "Assert files exist"
      assertPathsExist (toTestDir α `USeq.union` γTrash)
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTrash
  where
    desc = "Some trash entries are restored, others error"

emptyTrash :: Backend -> IO OsPath -> TestTree
emptyTrash backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Empties the trash" "empty" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|emptyTrash|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> [osp|.trash|]
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- empty trash
      usingIntIO env $ SafeRm.emptyTrash True

      -- get index
      index <- view #unIndex <$> usingIntIO env SafeRm.getIndex
      annotateShow index

      [] === index
      annotate "Assert files do not exist"
      assertPathsDoNotExist (αTest `USeq.union` αTrash)

metadata :: Backend -> IO OsPath -> TestTree
metadata backend mtestDir = askOption $ \(MkAsciiOnly b) -> do
  testPropertyNamed "Retrieves metadata" "metadata" $ do
    property $ do
      testDir <- getTestPath mtestDir [osp|metadata|] backend
      (α, _) <- forAll (IntUtils.genFileNameSet b)
      let αTest = USeq.map (testDir </>) α
          trashDir = testDir </> [osp|.trash|]
          αTrash = mkTrashPaths backend trashDir α
      env <- liftIO $ mkEnv backend trashDir

      annotateShow testDir
      annotateShow αTest

      -- create files and assert existence
      setupDir testDir αTest

      -- delete files
      usingIntIO env $ SafeRm.delete (USeq.map MkPathI αTest)

      -- assert original files moved to trash
      annotate "Assert files exist"
      assertPathsExist αTrash
      annotate "Assert files do not exist"
      assertPathsDoNotExist αTest

      -- get metadata
      metadata' <- usingIntIO env SafeRm.getMetadata

      length α === natToInt (metadata' ^. #numEntries)
      length α === natToInt (metadata' ^. #numFiles)

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

mkTrashPaths ::
  ( Foldable f,
    IsList (f OsPath),
    Item (f OsPath) ~ OsPath
  ) =>
  Backend ->
  OsPath ->
  f OsPath ->
  f OsPath
mkTrashPaths backend trashHome =
  fromList . foldr (\p acc -> mkTrashPath p : mkTrashInfoPath p : acc) []
  where
    mkTrashPath p = trashHome </> pathFiles </> p
    mkTrashInfoPath p = trashHome </> pathInfo </> p <> Env.trashInfoExtensionOsPath backend

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
  testPath <- liftIO $ canonicalizePath =<< mtestPath
  pure $ testPath </> p </> Backend.backendArgOsPath backend

setupDir ::
  ( MonadCatch m,
    MonadIO m,
    MonadTest m
  ) =>
  OsPath ->
  UniqueSeq OsPath ->
  m ()
setupDir dir files = do
  let action = do
        clearDirectory dir
        createFilesMap USeq.map files
        assertPathsExist files

  action `catchAny` \ex -> do
    annotate $ displayException ex
    failure
