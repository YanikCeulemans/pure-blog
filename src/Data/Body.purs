module Data.Body
  ( AssetBody
  , CssBody
  , HtmlBody
  , MimeType(..)
  , assetBody
  , cssBody
  , htmlBody
  ) where

import Prelude

import Data.Argonaut as Json
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Int as Int
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as String
import Data.Time.Duration (Days(..), Seconds(..), convertDuration)
import Data.Tuple (Tuple(..))
import HTTPure.Body (class Body)
import HTTPure.Body as HTTPureBody
import HTTPure.Headers as HTTPureHeaders
import Node.Buffer (Buffer)
import Node.Path (FilePath)

newtype HtmlBody = HtmlBody String

instance Body HtmlBody where
  defaultHeaders (HtmlBody htmlString) =
    append htmlHeader <$> HTTPureBody.defaultHeaders htmlString
    where
    htmlHeader = HTTPureHeaders.header "Content-Type" "text/html; charset=utf-8"
  write (HtmlBody htmlString) r = HTTPureBody.write htmlString r

instance Json.EncodeJson HtmlBody where
  encodeJson (HtmlBody htmlString) = Json.encodeJson htmlString

htmlBody :: String -> HtmlBody
htmlBody = HtmlBody

newtype CssBody = CssBody String

instance Body CssBody where
  write (CssBody s) r = HTTPureBody.write s r
  defaultHeaders (CssBody s) =
    HTTPureBody.defaultHeaders s
      <#> append cssHeader
    where
    cssHeader = HTTPureHeaders.header "Content-Type" "text/css; charset=utf-8"

cssBody :: String -> CssBody
cssBody = CssBody

data MimeType
  = TextCss
  | ImagePng

newtype AssetBody = AssetBody
  { mimeType :: Maybe MimeType
  , buffer :: Buffer
  }

assetBody :: FilePath -> Buffer -> AssetBody
assetBody filePath b = AssetBody { mimeType, buffer: b }
  where
  mimeType =
    String.split (String.Pattern ".") filePath
      # Array.reverse
      # List.fromFoldable
      # case _ of
          ext : _ -> mimeTypeFromExtension ext
          _ -> Nothing

  mimeTypeFromExtension "png" = Just ImagePng
  mimeTypeFromExtension "css" = Just TextCss
  mimeTypeFromExtension _ = Nothing

assetMaxAgeInSeconds :: Int
assetMaxAgeInSeconds =
  Days 1.0
    # convertDuration
    # un Seconds
    # Int.round

instance Body AssetBody where
  write (AssetBody { buffer }) r = HTTPureBody.write buffer r
  defaultHeaders (AssetBody { mimeType, buffer }) =
    HTTPureBody.defaultHeaders buffer
      <#> append assetHeaders
    where
    contentTypeMaybe = mimeType <#> case _ of
      TextCss -> "text/css; charset=utf-8"
      ImagePng -> "image/png"
    assetHeaders =
      [ contentTypeMaybe <#> Tuple "Content-Type"
      , Just $ Tuple "Cache-Control" $ "public, max-age=" <>
          show assetMaxAgeInSeconds
      ]
        # mapMaybe identity
        # HTTPureHeaders.headers
