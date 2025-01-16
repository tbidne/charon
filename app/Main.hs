-- | Main module.
module Main (main) where

import Charon.Runner (runCharon)
import Charon.Utils qualified as Utils
import Control.Exception.Annotation.Utils qualified as AnnUtils

main :: IO ()
main = do
  AnnUtils.setIgnoreKnownCallStackHandler Utils.noCallstacks

  runCharon
