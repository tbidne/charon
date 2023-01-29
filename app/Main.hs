-- | Main module.
--
-- @since 0.1
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude
  ( ExceptionCS (..),
    catchCS,
    displayException,
    throwCS,
  )
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  runSafeRm
    `catchCS` \case
      -- For optparse applicative
      (MkExceptionCS ExitSuccess _) -> pure ()
      other -> throwCS other
