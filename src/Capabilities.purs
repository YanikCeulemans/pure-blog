module Capabilities (class MonadLog, log, LogLevel) where

import Prelude

import Data.Argonaut (Json)
import Data.Maybe (Maybe)

data LogLevel
  = Debug
  | Info
  | Warn
  | Error

class Monad m <= MonadLog m where
  log :: LogLevel -> String -> Maybe Json -> m Unit
