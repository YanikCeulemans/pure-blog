module Data.Body
  ( AssetBody
  , CssBody
  , MimeType(..)
  , assetBody
  , cssBodyFromString
  ) where

import Prelude

import Data.Array (mapMaybe)
import Data.Array as Array
import Data.List ((:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import HTTPure.Body (class Body)
import HTTPure.Body as HTTPureBody
import HTTPure.Headers as HTTPureHeaders
import Node.Buffer (Buffer)
import Node.Path (FilePath)

newtype CssBody = CssBody String

instance Body CssBody where
  write (CssBody s) r = HTTPureBody.write s r
  defaultHeaders (CssBody s) =
    HTTPureBody.defaultHeaders s
      <#> append cssHeader
    where
    cssHeader = HTTPureHeaders.header "Content-Type" "text/css; charset=utf-8"

cssBodyFromString :: String -> CssBody
cssBodyFromString = CssBody

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
      [ contentTypeMaybe <#> Tuple "Content-Type" ]
        # mapMaybe identity
        # HTTPureHeaders.headers
