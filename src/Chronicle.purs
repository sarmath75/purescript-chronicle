module Chronicle where

import Prelude

import Control.Logger (Logger(..), cfilter)
import Control.Logger as Logger
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (fromRight)
import Data.Formatter.DateTime as FormatterDateTime
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafePartial)

data ChronicleLevel
  = DEBUG
  | INFO
  | WARN
  | ERROR

derive instance eqChronicleLevel :: Eq ChronicleLevel

derive instance ordChronicleLevel :: Ord ChronicleLevel

derive instance genericChronicleLevel :: Generic ChronicleLevel _

instance showChronicleLevel :: Show ChronicleLevel where
  show = genericShow

newtype ChronicleEntry = ChronicleEntry
  { instant :: Instant
  , level :: ChronicleLevel
  , file :: String
  , line :: Int
  , message :: String }

derive instance eqChronicleEntry :: Eq ChronicleEntry

derive instance ordChronicleEntry :: Ord ChronicleEntry

derive instance genericChronicleEntry :: Generic ChronicleEntry _

instance showChronicleEntry :: Show ChronicleEntry where
  show (ChronicleEntry {instant, level, file, line, message}) =
    formatInstant instant <> " " <> show level <> " " <> file <> ":" <> show line <> " " <> message
    where
      formatInstant :: Instant -> String
      formatInstant x =
        let formatter = unsafePartial $ fromRight $ FormatterDateTime.parseFormatString "YYYY-MM-DDTHH:mm:ss:SSSZ"
        in
          FormatterDateTime.format formatter $ Instant.toDateTime x

type ChronicleLocation =
  { file :: String
  , line :: Int }

type ChronicleEffs eff =
  ( console :: CONSOLE
  , now :: NOW
  , ref :: REF
  | eff )

type ChronicleEnv eff = {logger :: Logger (Eff (ChronicleEffs eff)) ChronicleEntry}

type ChronicleOpts eff =
  { logger :: Logger (Eff (ChronicleEffs eff)) ChronicleEntry
  , root :: ChronicleLevel
  , levels :: Array (Tuple String ChronicleLevel) }

-- | Resets environment.
reset :: forall eff. ChronicleOpts eff -> Eff (ChronicleEffs eff) Unit
reset {logger, root, levels} = do
  let logger' = cfilter predicate logger
  writeRef __env__ {logger: logger'}
  where
    predicate :: ChronicleEntry -> Boolean
    predicate (ChronicleEntry {level, file}) =
      level >= (fromMaybe root $ Map.lookup file entries)
      where
        entries = Map.fromFoldable levels

-- | Default options.
defaultOpts :: forall eff. ChronicleOpts eff
defaultOpts =
  { logger: consoleLogger show
  , root: WARN
  , levels: [] }

-- | Write a message.
log :: forall eff. ChronicleLocation -> String -> Eff (ChronicleEffs eff) Unit
log location message = log' DEBUG location message

-- | Write a value using its `Show` instance to produce a `String`.
logShow :: forall a eff. Show a => ChronicleLocation -> a -> Eff (ChronicleEffs eff) Unit
logShow location a = log' DEBUG location (show a)

-- | Write an info message.
info :: forall eff. ChronicleLocation -> String -> Eff (ChronicleEffs eff) Unit
info location message = log' INFO location message

-- | Write an info value using its `Show` instance to produce a `String`.
infoShow :: forall a eff. Show a => ChronicleLocation ->  a -> Eff (ChronicleEffs eff) Unit
infoShow location a = log' INFO location (show a)

-- | Write a warning message.
warn :: forall eff. ChronicleLocation -> String -> Eff (ChronicleEffs eff) Unit
warn location message = log' WARN location message

-- | Write an warning value, using its `Show` instance to produce a `String`.
warnShow :: forall a eff. Show a => ChronicleLocation -> a -> Eff (ChronicleEffs eff) Unit
warnShow location a = log' WARN location (show a)

-- | Write an error message.
error :: forall eff. ChronicleLocation -> String -> Eff (ChronicleEffs eff) Unit
error location message = log' ERROR location message

-- | Write an error value, using its `Show` instance to produce a `String`.
errorShow :: forall a eff. Show a => ChronicleLocation -> a -> Eff (ChronicleEffs eff) Unit
errorShow location a = log' ERROR location (show a)

-- | Write a message.
log' :: forall eff. ChronicleLevel -> ChronicleLocation -> String -> Eff (ChronicleEffs eff) Unit
log' level {file, line} message = do
  {logger} <- readRef __env__
  instant <- liftEff now
  liftEff $ Logger.log logger (ChronicleEntry {instant, level, file, line, message})

consoleLogger :: forall eff. (ChronicleEntry -> String) -> Logger (Eff (console :: CONSOLE | eff)) ChronicleEntry
consoleLogger format = Logger \entry@(ChronicleEntry {level}) -> case level of
  DEBUG ->
   Console.log $ format entry
  INFO ->
   Console.info $ format entry
  WARN ->
    Console.warn $ format entry
  ERROR ->
    Console.error $ format entry

listLogger :: forall eff. Ref (List ChronicleEntry) -> Logger (Eff (ref :: REF | eff)) ChronicleEntry
listLogger ref = Logger \entry -> do
  modifyRef ref \entries -> List.snoc entries entry

__env__ :: forall eff. Ref (ChronicleEnv eff)
__env__ = unsafePerformEff $ newRef {logger: consoleLogger show}
