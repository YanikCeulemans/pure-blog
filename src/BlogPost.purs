module BlogPost
  ( RawBlogPostMetaData
  , BlogPostMetaData
  , BlogPostDecodeError
  , BlogPost
  , blogPost
  , content
  , metaData
  , fromRawBlogPostMetaData
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

newtype BlogPost = BlogPost
  { metaData :: BlogPostMetaData
  , content :: String
  }

blogPost :: BlogPostMetaData -> String -> BlogPost
blogPost m c = BlogPost { metaData: m, content: c }

content :: BlogPost -> String
content (BlogPost { content: c }) = c

metaData :: BlogPost -> BlogPostMetaData
metaData (BlogPost { metaData: m }) = m

newtype RawBlogPostMetaData = RawBlogPostMetaData
  { title :: String
  , summary :: String
  , slug :: String
  , timestamp :: String
  }

instance Json.DecodeJson RawBlogPostMetaData where
  decodeJson = Json.decodeJson >>> map RawBlogPostMetaData

newtype BlogPostMetaData = BlogPostMetaData
  { title :: String
  , summary :: String
  , slug :: Slug
  , timestamp :: Date
  }

slug :: BlogPostMetaData -> Slug
slug (BlogPostMetaData { slug: s }) = s

data BlogPostDecodeError
  = InvalidDate String
  | InvalidSlug String

printBlogPostDecodeError :: BlogPostDecodeError -> String
printBlogPostDecodeError (InvalidDate dateString) =
  "Invalid date: " <> dateString
printBlogPostDecodeError (InvalidSlug slugString) =
  "Invalid slug: " <> slugString

fromRawBlogPostMetaData
  :: RawBlogPostMetaData -> Either BlogPostDecodeError BlogPostMetaData
fromRawBlogPostMetaData
  (RawBlogPostMetaData { title, summary, slug: slugS, timestamp: timestampS }) =
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
      $ BlogPostMetaData
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

instance Json.EncodeJson BlogPostMetaData where
  encodeJson
    (BlogPostMetaData { title, summary, slug: slugT, timestamp: timestampD }) =
    let
      timestampDT = DateTime timestampD $ Time bottom bottom bottom bottom
      timestamp = Format.format timestampFormatter timestampDT
      slugS = Slug.toString slugT
    in
      Json.encodeJson
        { title, summary, slug: slugS, timestamp }
