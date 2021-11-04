module Data.Log
  ( LogLevel(..)
  , Log
  , parseLogLevel
  , message
  , level
  , mkLog
  ) where

import Prelude

import Capabilities.Now (class Now, nowDateTime)
import Data.Argonaut as Json
import Data.Array (fold, intercalate)
import Data.DateTime as DateTime
import Data.Either (fromRight)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))

data LogLevel
  = Debug
  | Info
  | Warn
  | Error

derive instance Eq LogLevel
derive instance Ord LogLevel

newtype Log =
  Log
    { timestamp :: DateTime.DateTime
    , level :: LogLevel
    , message :: String
    , data :: Maybe Json.Json
    }

message :: Log -> String
message (Log { message: m }) = m

level :: Log -> LogLevel
level (Log { level: l }) = l

mkLog :: forall m. Now m => LogLevel -> String -> m Log
mkLog lvl msg = do
  timestamp <- nowDateTime

  let
    formattedTimestamp = fold [ "[", formatTimestamp timestamp, "]" ]
    formattedLevel = fold [ "[" <> formatLevel lvl <> "]" ]
    formattedMessage = intercalate " "
      [ formattedTimestamp, formattedLevel, msg ]
  pure
    $ Log
        { timestamp
        , level: lvl
        , data: Nothing
        , message: formattedMessage
        }
  where
  formatTimestamp =
    formatDateTime "YYYY-MM-DD hh:mm:ss.SSS"
      >>> fromRight "Invalid date format string"
  formatLevel =
    case _ of
      Debug -> "DEBUG"
      Info -> "INFO"
      Warn -> "WARN"
      Error -> "ERROR"

parseLogLevel :: String -> Maybe LogLevel
parseLogLevel = case _ of
  "DEBUG" -> Just Debug
  "INFO" -> Just Info
  "WARN" -> Just Warn
  "ERROR" -> Just Error
  _ -> Nothing

