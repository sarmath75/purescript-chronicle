module ChronicleSpec where

import Prelude

import Chronicle (ChronicleEntry(..), ChronicleLevel(..), ChronicleLocation, defaultOpts, error, errorShow, info, infoShow, listLogger, log, logShow, reset, warn, warnShow)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF, newRef, readRef)
import Data.Foldable (for_, length)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type ChronicleSpecEffs eff =
  ( console :: CONSOLE
  , now :: NOW
  , ref :: REF
  | eff)

spec :: forall eff. Spec (ChronicleSpecEffs eff) Unit
spec = do
  describe "ChronicleSpec" do
    it "should [log|info|warn|error]" do
      let xs = [ {level: DEBUG, f: log}
               , {level: INFO, f: info}
               , {level: WARN, f: warn}
               , {level: ERROR, f: error} ]
      for_ xs \{level, f} -> do
        ref <- liftEff $ newRef Nil
        liftEff $ reset $ defaultOpts {root = level, logger = listLogger ref}
        liftEff $ f __LOC__ "true"
        entries <- liftEff $ readRef ref
        shouldEqual 1 (length entries)
        let (ChronicleEntry {level, file, line, message}) = unsafePartial $ fromJust $ List.head entries
        shouldEqual level level
        shouldEqual __LOC__.file file
        shouldEqual __LOC__.line line
        shouldEqual "true" message
    it "should [log|info|warn|error]Show" do
      let xs = [ {level: DEBUG, f: logShow}
               , {level: INFO, f: infoShow}
               , {level: WARN, f: warnShow}
               , {level: ERROR, f: errorShow} ]
      for_ xs \{level, f} -> do
        ref <- liftEff $ newRef Nil
        liftEff $ reset $ defaultOpts {root = level, logger = listLogger ref}
        liftEff $ f __LOC__ true
        entries <- liftEff $ readRef ref
        shouldEqual 1 (length entries)
        let (ChronicleEntry {level, file, line, message}) = unsafePartial $ fromJust $ List.head entries
        shouldEqual level level
        shouldEqual __LOC__.file file
        shouldEqual __LOC__.line line
        shouldEqual "true" message
    it "should filter" do
      let xs = [ {level: INFO, f: log}
               , {level: WARN, f: info}
               , {level: ERROR, f: warn} ]
      for_ xs \{level, f} -> do
        ref <- liftEff $ newRef Nil
        liftEff $ reset $ defaultOpts {root = level, logger = listLogger ref}
        liftEff $ f __LOC__ "test"
        entries <- liftEff $ readRef ref
        shouldEqual 0 (length entries)

__LOC__ :: ChronicleLocation
__LOC__ = {file: "ChronicleSpec", line: 0}
