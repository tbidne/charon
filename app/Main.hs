-- | Main module.
module Main (main) where

import Effectful (runEff)
import Effectful.Concurrent (runConcurrent)
import Effectful.FileSystem.FileReader.Dynamic (runFileReaderDynamicIO)
import Effectful.FileSystem.FileWriter.Dynamic (runFileWriterDynamicIO)
import Effectful.FileSystem.HandleWriter.Dynamic (runHandleWriterDynamicIO)
import Effectful.FileSystem.PathReader.Dynamic (runPathReaderDynamicIO)
import Effectful.FileSystem.PathWriter.Dynamic (runPathWriterDynamicIO)
import Effectful.IORef.Static (runIORefStaticIO)
import Effectful.Optparse.Static (runOptparseStaticIO)
import Effectful.PosixCompat.Static (runPosixCompatStaticIO)
import Effectful.Terminal.Dynamic (runTerminalDynamicIO)
import Effectful.Time.Dynamic (runTimeDynamicIO)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude
  ( catch,
    displayException,
    throwM,
  )
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  run runSafeRm
    `catch` \case
      -- For optparse applicative
      ExitSuccess -> pure ()
      -- We throw ExitFailure for a number of errors in SafeRm. In these cases
      -- we do not want to print the CallStack as it is just noise. We only
      -- want callstacks for other errors i.e. unexpected ones.
      ex@(ExitFailure _) -> throwM ex
  where
    run =
      runEff
        . runConcurrent
        . runFileReaderDynamicIO
        . runFileWriterDynamicIO
        . runHandleWriterDynamicIO
        . runIORefStaticIO
        . runOptparseStaticIO
        . runPathReaderDynamicIO
        . runPathWriterDynamicIO
        . runPosixCompatStaticIO
        . runTerminalDynamicIO
        . runTimeDynamicIO
