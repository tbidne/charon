-- | Main module.
module Main (main) where

import Charon.Prelude
  ( ExceptionCS (MkExceptionCS),
    catch,
    displayException,
    throwM,
  )
import Charon.Runner (runCharon)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  runCharon
    `catch` \case
      -- For optparse applicative
      (MkExceptionCS ExitSuccess _) -> pure ()
      -- We throw ExitFailure for a number of errors in Charon. In these cases
      -- we do not want to print the CallStack as it is just noise. We only
      -- want callstacks for other errors i.e. unexpected ones.
      (MkExceptionCS ex@(ExitFailure _) _) -> throwM ex
