-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception (fromException)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude (displayCallStack)
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex ->
    case fromException ex of
      Just ExitSuccess -> pure ()
      _ -> putStrLn (displayCallStack ex)

  runSafeRm
