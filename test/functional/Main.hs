-- | Entrypoint for functional tests.
--
-- @since 0.1
module Main (main) where

import Effects.MonadFsWriter
  ( MonadFsWriter
      ( createDirectoryIfMissing,
        removePathForcibly
      ),
  )
import Functional.Commands.D qualified as D
import Functional.Commands.E qualified as E
import Functional.Commands.L qualified as L
import Functional.Commands.M qualified as M
import Functional.Commands.R qualified as R
import Functional.Commands.X qualified as X
import Functional.Logging qualified as Logging
import Functional.Prelude
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty qualified as Tasty
import UnliftIO.Directory qualified as Dir

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.withResource setup teardown $ \args ->
      testGroup
        "Functional Tests"
        [ D.tests args,
          X.tests args,
          E.tests args,
          R.tests args,
          L.tests args,
          M.tests args,
          Logging.tests args
        ]

setup :: IO FilePath
setup = do
  tmpDir <- (</> "safe-rm/functional") <$> Dir.getTemporaryDirectory
  createDirectoryIfMissing True tmpDir
  pure tmpDir

teardown :: FilePath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = removePathForcibly fp
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> fp
