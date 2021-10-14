module Styles.Main where

import Prelude

import CSS (CSS, GenericFontFamily(..), backgroundColor, bold, color, element, fontFamily, fontSize, fontWeight, fromInt, fromString, margin, marginBottom, nil, px, rem, select, star, (?))
import Data.NonEmpty as NE

main :: CSS
main = do
  element "html" ? do
    fontSize $ px 10.0

  element "body" ? do
    fontSize $ rem 2.2
    fontFamily
      [ "Crimson Text" ]
      $ NE.NonEmpty
        (GenericFontFamily $ fromString "Times")
        [ GenericFontFamily $ fromString "Serif" ]
    backgroundColor $ fromInt 0x282c34
    color $ fromInt 0xdcdfe4

  element "h1" ? do
    fontSize $ rem 5.2
    fontWeight $ bold
    marginBottom $ rem 2.0

  element "p" ? do
    margin (rem 1.0) nil (rem 1.0) nil

