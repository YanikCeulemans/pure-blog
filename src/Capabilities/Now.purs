module Capabilities.Now (class Now, now, nowDate, nowTime, nowDateTime) where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime, Time)
import Data.DateTime.Instant (Instant)

class Monad m <= Now m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime
