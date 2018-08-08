module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, run')

main :: forall eff. Effect Unit
main = do
  let pattern = "Chronicle*Spec"
      config = defaultConfig
      reporters = [consoleReporter]
  discover pattern >>= run' config reporters
