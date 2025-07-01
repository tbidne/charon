-- | Main module.
module Main (main) where

import Charon.Prelude (runReaderT)
import Charon.Runner (runCharon)
import Charon.Utils qualified as Utils
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Effects.Haskeline qualified as EH

main :: IO ()
main = do
  AnnUtils.setIgnoreKnownCallStackHandler Utils.noCallstacks
  EH.runInputTEnv (runReaderT runCharon)
