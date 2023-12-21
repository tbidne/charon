-- | Main module.
module Main (main) where

import Charon.Runner (runCharon)
import Charon.Utils qualified as Utils
import Effects.Exception qualified as Ex

main :: IO ()
main = do
  Ex.setUncaughtExceptionDisplayCSNoMatch
    Utils.noCallstacks
    (putStrLn . ("\n" <>))

  runCharon
