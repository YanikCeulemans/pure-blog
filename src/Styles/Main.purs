module Styles.Main where

import Prelude

import CSS (CSS, GenericFontFamily(..), Size(..), alignItems, backgroundColor, bold, byClass, color, display, element, flex, fontFamily, fontSize, fontWeight, fromInt, fromString, grid, key, margin, marginBottom, nil, padding, px, rem, star, (?), (|*))
import CSS.Common (auto, center)
import CSS.Selector (with)
import Data.NonEmpty as NE
import Data.Tuple.Nested (tuple2)

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
    display grid
    key (fromString "gap") $ rem 2.0
    key (fromString "grid-template-rows") $ tuple2 (rem 5.0) (auto :: Size _)

  element "h1" ? do
    fontSize $ rem 5.2
    fontWeight $ bold
    marginBottom $ rem 2.0

  element "p" ? do
    margin (rem 1.0) nil (rem 1.0) nil

  element "header" ? do
    display flex
    alignItems center
    padding nil (rem 2.0) nil (rem 2.0)
    backgroundColor $ fromInt 0x61afef
    fontWeight bold
    fontFamily
      [ "Helvetica" ]
      $ NE.NonEmpty
          (GenericFontFamily $ fromString "sans-serif")
          []

  element "header" |* element "a" ? do
    color $ fromInt 0x333333

  star `with` byClass "main-content" ? do
    padding nil (rem 2.0) nil (rem 2.0)
