-- | Main module.
--
-- @since 0.1
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude
  ( ExceptionCS (..),
    catchWithCS,
    displayException,
    throwWithCS,
  )
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayException ex)

  runSafeRm
    `catchWithCS` \case
      -- For optparse applicative
      (MkExceptionCS ExitSuccess _) -> pure ()
      other -> throwWithCS other
