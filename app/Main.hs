-- | Main module.
--
-- @since 0.1
module Main (main) where

import GHC.Conc.Sync (setUncaughtExceptionHandler)
import SafeRm.Prelude
  ( AnnotatedException (..),
    catchWithCallStack,
    displayCallStack,
    throwWithCallStack,
  )
import SafeRm.Runner (runSafeRm)
import System.Exit (ExitCode (..))

main :: IO ()
main = do
  setUncaughtExceptionHandler $ \ex -> putStrLn ("\n" <> displayCallStack ex)

  runSafeRm
    `catchWithCallStack` \case
      -- For optparse applicative
      (AnnotatedException _ ExitSuccess) -> pure ()
      other -> throwWithCallStack other
