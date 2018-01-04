module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Node.FS (FS)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, defaultConfig, run')

main :: forall eff. Eff (RunnerEffects (fs :: FS | eff)) Unit
main = do
  let pattern = "Chronicle*Spec"
      config = defaultConfig
      reporters = [consoleReporter]
  discover pattern >>= run' config reporters
