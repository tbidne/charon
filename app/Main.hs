-- | Main module.
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude
  ( ExceptionCS (..),
    catch,
    displayException,
    throwM,
  )
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  runSafeRm
    `catch` \case
      -- For optparse applicative
      (MkExceptionCS ExitSuccess _) -> pure ()
      -- We throw ExitFailure for a number of errors in SafeRm. In these cases
      -- we do not want to print the CallStack as it is just noise. We only
      -- want callstacks for other errors i.e. unexpected ones.
      (MkExceptionCS ex@(ExitFailure _) _) -> throwM ex
