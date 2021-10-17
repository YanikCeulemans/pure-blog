module Slug (Slug, fromString, toString) where

import Prelude

import Data.Either (fromRight')
import Data.Maybe (Maybe(..))
import Data.String.Regex as Regex
import Data.String.Regex as String
import Data.String.Regex.Flags as RegexFlags
import Partial.Unsafe (unsafeCrashWith)

newtype Slug = Slug String

derive newtype instance Eq Slug

slugRegex :: Regex.Regex
slugRegex =
  Regex.regex "^(?:[a-z-])*[a-z]+$" RegexFlags.noFlags
    # fromRight' (\_ -> unsafeCrashWith "Invalid slugRegex")

fromString :: String -> Maybe Slug
fromString s =
  if String.test slugRegex s then
    pure $ Slug s
  else
    Nothing

toString :: Slug -> String
toString (Slug s) = s

