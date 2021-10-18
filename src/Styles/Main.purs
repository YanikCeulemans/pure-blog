module Styles.Main where

import Prelude

import CSS (class Val, Abs, CSS, Color, Feature(..), GenericFontFamily(..), MediaQuery(..), Path(..), Refinement(..), Selector(..), Size, alignItems, backgroundColor, block, bold, border, byClass, color, column, display, element, flex, flexDirection, fontFamily, fontSize, fontStyle, fontWeight, fromInt, fromString, grid, key, lineHeight, margin, marginBottom, maxWidth, nil, padding, pct, pseudo, px, query, rem, solid, star, value, (&), (?), (|*))
import CSS.Common (auto, center)
import CSS.FontStyle (italic)
import CSS.Media (screen)
import CSS.Overflow (overflow, overflowAuto)
import CSS.Overflow as OverFlow
import Data.Maybe (Maybe(..))
import Data.NonEmpty as NE
import Data.Tuple.Nested (tuple2)

nearlyWhite :: Color
nearlyWhite = fromInt 0xdcdfe4

gap :: forall a. Size a -> CSS
gap = key (fromString "gap")

gridTemplateRows :: forall a. Val a => a -> CSS
gridTemplateRows =
  key (fromString "grid-template-rows")

mediaMinWidth :: Size Abs -> Feature
mediaMinWidth = Feature "min-width" <<< Just <<< value

mediaQueryLarge :: MediaQuery
mediaQueryLarge =
  MediaQuery Nothing screen $ NE.NonEmpty (mediaMinWidth $ px 930.0) []

queryLarge :: CSS -> CSS
queryLarge = query screen (NE.NonEmpty (mediaMinWidth $ px 930.0) [])

typography :: CSS
typography = do
  element "h1" ? do
    fontSize $ rem 5.2
    fontWeight $ bold
    marginBottom $ rem 2.0

  element "h3" ? do
    fontSize $ rem 4.2
    fontWeight $ bold
    marginBottom $ rem 1.5

  element "h4" ? do
    fontSize $ rem 3.2
    marginBottom $ rem 1.5

  element "p" ? do
    margin (rem 1.0) nil (rem 1.0) nil

  element "strong" ? do
    fontWeight bold

  element "em" ? do
    fontStyle italic

  element "code" ? do
    fontFamily
      [ "Fira Code" ]
      $ NE.NonEmpty (GenericFontFamily $ fromString "monospace") []
    lineHeight $ rem 3.0

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

  typography

  element "pre" ? do
    margin (rem 2.0) nil (rem 2.0) nil
    padding (rem 2.0) (rem 2.0) (rem 2.0) (rem 2.0)
    border solid (px 1.0) nearlyWhite
    overflow overflowAuto

  element "code" ? do
    display block

  element "img" ? do
    maxWidth $ pct 100.0

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
    overflow OverFlow.hidden

  star & byClass "post-date" ? do
    color $ fromInt 0xaaaaaa

  queryLarge do
    star & byClass "main-content" ? do
      maxWidth $ px 900.0
      margin nil auto nil auto

