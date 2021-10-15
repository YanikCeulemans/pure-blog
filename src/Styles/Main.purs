module Styles.Main where

import Prelude

import CSS (class Val, CSS, Color, GenericFontFamily(..), Path(..), Refinement(..), Selector(..), Size(..), alignItems, backgroundColor, bold, byClass, color, column, display, element, flex, flexDirection, fontFamily, fontSize, fontWeight, fromInt, fromString, grid, key, margin, marginBottom, nil, padding, pseudo, px, rem, star, (&), (?), (|*))
import CSS.Common (auto, center)
import Data.NonEmpty as NE
import Data.Tuple.Nested (tuple2)

nearlyWhite :: Color
nearlyWhite = fromInt 0xdcdfe4

gap :: forall a. Size a -> CSS
gap = key (fromString "gap")

gridTemplateRows :: forall a. Val a => a -> CSS
gridTemplateRows =
  key (fromString "grid-template-rows")

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
    color nearlyWhite
    display grid
    gap $ rem 2.0
    gridTemplateRows $ tuple2 (rem 5.0) (auto :: Size _)

  element "h1" ? do
    fontSize $ rem 5.2
    fontWeight $ bold
    marginBottom $ rem 2.0

  element "h3" ? do
    fontSize $ rem 4.2
    fontWeight $ bold
    marginBottom $ rem 1.5

  element "p" ? do
    margin (rem 1.0) nil (rem 1.0) nil

  Selector (Refinement [])
    (Combined (element "a") (element "a" & pseudo "hover")) ? do
    color nearlyWhite

  element "ol" ? do
    display flex
    flexDirection column
    gap $ rem 1.0

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

  star & byClass "main-content" ? do
    padding nil (rem 2.0) nil (rem 2.0)

  star & byClass "post-date" ? do
    color $ fromInt 0xaaaaaa
