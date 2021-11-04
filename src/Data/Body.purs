module Data.Body (CssBody, cssBodyFromString) where

import Prelude

import HTTPure.Body (class Body)
import HTTPure.Body as HTTPureBody
import HTTPure.Headers as HTTPureHeaders

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
