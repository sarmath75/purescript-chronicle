module Chronicle where

import Prelude

import Control.Logger (Logger(..), cfilter)
import Control.Logger as Logger
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
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

type ChronicleEnv = {logger :: Logger Effect ChronicleEntry}

type ChronicleOpts =
  { logger :: Logger Effect ChronicleEntry
  , root :: ChronicleLevel
  , levels :: Array (Tuple String ChronicleLevel) }

-- | Resets environment.
reset :: ChronicleOpts -> Effect Unit
reset {logger, root, levels} = do
  let logger' = cfilter predicate logger
  Ref.write {logger: logger'} __env__
  where
    predicate :: ChronicleEntry -> Boolean
    predicate (ChronicleEntry {level, file}) =
      level >= (fromMaybe root $ Map.lookup file entries)
      where
        entries = Map.fromFoldable levels

-- | Default options.
defaultOpts :: ChronicleOpts
defaultOpts =
  { logger: consoleLogger show
  , root: WARN
  , levels: [] }

-- | Write a message.
log :: ChronicleLocation -> String -> Effect Unit
log location message = log' DEBUG location message

-- | Write a value using its `Show` instance to produce a `String`.
logShow :: forall a. Show a => ChronicleLocation -> a -> Effect Unit
logShow location a = log' DEBUG location (show a)

-- | Write an info message.
info :: ChronicleLocation -> String -> Effect Unit
info location message = log' INFO location message

-- | Write an info value using its `Show` instance to produce a `String`.
infoShow :: forall a. Show a => ChronicleLocation ->  a -> Effect Unit
infoShow location a = log' INFO location (show a)

-- | Write a warning message.
warn :: ChronicleLocation -> String -> Effect Unit
warn location message = log' WARN location message

-- | Write an warning value, using its `Show` instance to produce a `String`.
warnShow :: forall a. Show a => ChronicleLocation -> a -> Effect Unit
warnShow location a = log' WARN location (show a)

-- | Write an error message.
error :: ChronicleLocation -> String -> Effect Unit
error location message = log' ERROR location message

-- | Write an error value, using its `Show` instance to produce a `String`.
errorShow :: forall a. Show a => ChronicleLocation -> a -> Effect Unit
errorShow location a = log' ERROR location (show a)

-- | Write a message.
log' :: ChronicleLevel -> ChronicleLocation -> String -> Effect Unit
log' level {file, line} message = do
  {logger} <- Ref.read __env__
  instant <- liftEffect now
  liftEffect $ Logger.log logger (ChronicleEntry {instant, level, file, line, message})

consoleLogger :: (ChronicleEntry -> String) -> Logger Effect ChronicleEntry
consoleLogger format = Logger \entry@(ChronicleEntry {level}) -> case level of
  DEBUG ->
   Console.log $ format entry
  INFO ->
   Console.info $ format entry
  WARN ->
    Console.warn $ format entry
  ERROR ->
    Console.error $ format entry

listLogger :: Ref (List ChronicleEntry) -> Logger Effect ChronicleEntry
listLogger ref = Logger \entry -> do
  Ref.modify_ (\entries -> List.snoc entries entry) ref

__env__ :: Ref ChronicleEnv
__env__ = unsafePerformEffect $ Ref.new {logger: consoleLogger show}
