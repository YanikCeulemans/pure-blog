module Data.Environment (Environment(..), parseEnvironment) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.CaseInsensitive (CaseInsensitiveString(..))

data Environment
  = Development
  | Test
  | Production

instance Show Environment where
  show = case _ of
    Development -> "Development"
    Test -> "Test"
    Production -> "Production"

matchesAny :: Array String -> String -> Boolean
matchesAny targets source =
  targets
    # map CaseInsensitiveString
    # Array.any (eq $ CaseInsensitiveString source)

parseEnvironment :: String -> Maybe Environment
parseEnvironment s
  | matchesAny [ "prod", "production" ] s = Just Production
  | matchesAny [ "tst", "test" ] s = Just Test
  | matchesAny [ "dev", "devlocal", "local", "development" ] s =
      Just Development
  | otherwise = Nothing
