module BlogPost
  ( RawBlogPost
  , BlogPost
  , BlogPostDecodeError
  , fromRawBlogPost
  , printBlogPostDecodeError
  , slug
  ) where

import Prelude

import Data.Argonaut as Json
import Data.Date (Date, exactDate)
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..), note)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Formatter.DateTime as Format
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe)
import Data.String as S
import Slug (Slug)
import Slug as Slug

newtype RawBlogPost = RawBlogPost
  { title :: String
  , summary :: String
  , slug :: String
  , timestamp :: String
  }

instance Json.DecodeJson RawBlogPost where
  decodeJson = Json.decodeJson >>> map RawBlogPost

newtype BlogPost = BlogPost
  { title :: String
  , summary :: String
  , slug :: Slug
  , timestamp :: Date
  }

slug :: BlogPost -> Slug
slug (BlogPost { slug: s }) = s

data BlogPostDecodeError
  = InvalidDate String
  | InvalidSlug String

printBlogPostDecodeError :: BlogPostDecodeError -> String
printBlogPostDecodeError (InvalidDate dateString) =
  "Invalid date: " <> dateString
printBlogPostDecodeError (InvalidSlug slugString) =
  "Invalid slug: " <> slugString

fromRawBlogPost :: RawBlogPost -> Either BlogPostDecodeError BlogPost
fromRawBlogPost
  (RawBlogPost { title, summary, slug: slugS, timestamp: timestampS }) =
  do
    timestamp <-
      case S.split (S.Pattern "-") timestampS of
        [ yearS, monthS, dayS ] ->
          note (InvalidDate timestampS) do
            year <- stringToEnum yearS
            month <- stringToEnum monthS
            day <- stringToEnum dayS
            exactDate year month day
        _ -> Left $ InvalidDate timestampS
    validatedSlug <- note (InvalidSlug slugS) $ Slug.fromString slugS
    pure
      $ BlogPost
          { title
          , summary
          , slug: validatedSlug
          , timestamp
          }
  where
  stringToEnum :: forall a. BoundedEnum a => String -> Maybe a
  stringToEnum = Int.fromString >=> toEnum

timestampFormatter :: Format.Formatter
timestampFormatter = List.fromFoldable
  [ Format.YearFull
  , Format.Placeholder "-"
  , Format.MonthTwoDigits
  , Format.Placeholder "-"
  , Format.DayOfMonthTwoDigits
  ]

instance Json.EncodeJson BlogPost where
  encodeJson (BlogPost { title, summary, slug: slugT, timestamp: timestampD }) =
    let
      timestampDT = DateTime timestampD $ Time bottom bottom bottom bottom
      timestamp = Format.format timestampFormatter timestampDT
      slugS = Slug.toString slugT
    in
      Json.encodeJson
        { title, summary, slug: slugS, timestamp }
