module Capabilities.LogMessages
  ( class LogMessages
  , logMessage
  ) where

import Prelude

import Capabilities.Now (class Now)
import Data.Log (Log, LogLevel(..), mkLog)

class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

log :: forall m. LogMessages m => Now m => LogLevel -> String -> m Unit
log lvl = mkLog lvl >=> logMessage

logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error
