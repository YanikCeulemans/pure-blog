module Styles.Main where

import Prelude

import CSS (class Val, Abs, CSS, Color, Feature(..), GenericFontFamily(..), Path(..), Refinement(..), Selector(..), Size, alignItems, backgroundColor, black, block, bold, border, byClass, color, column, display, element, flex, flexDirection, fontFamily, fontSize, fontStyle, fontWeight, fromInt, fromString, grid, hover, inlineBlock, key, letterSpacing, lighter, lineHeight, margin, marginBottom, maxWidth, nil, noneTextDecoration, padding, paddingLeft, pct, pseudo, px, query, rem, solid, star, textDecoration, textTransform, textWhitespace, value, white, whitespacePreWrap, (&), (?), (|*))
import CSS.Common (auto, center)
import CSS.FontStyle (italic)
import CSS.ListStyle.Type (decimal, disc, listStyleType)
import CSS.Media (screen)
import CSS.Overflow (overflow, overflowAuto)
import CSS.Overflow as OverFlow
import CSS.Text.Transform (uppercase)
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

queryLarge :: CSS -> CSS
queryLarge = query screen $ NE.NonEmpty (mediaMinWidth $ px 930.0) []

typography :: CSS
typography = do
  element "h1" ? do
    fontSize $ rem 5.2
    fontWeight $ bold
    marginBottom $ rem 2.0

  element "h2" ? do
    fontSize $ rem 3.6
    fontWeight $ bold
    margin (rem 4.0) nil (rem 2.0) nil

  element "h3" ? do
    fontSize $ rem 3.0
    fontWeight $ bold
    marginBottom $ rem 1.5

  element "h4" ? do
    fontSize $ rem 2.8
    marginBottom $ rem 1.5

  element "p" ? do
    margin (rem 2.0) nil (rem 2.0) nil
    lineHeight $ rem 3.0

  element "strong" ? do
    fontWeight bold

  element "em" ? do
    fontStyle italic

  element "code" ? do
    fontFamily
      [ "Fira Code" ]
      $ NE.NonEmpty (GenericFontFamily $ fromString "monospace") []
    lineHeight $ rem 2.3
    fontSize $ rem 1.6

main :: CSS
main = do
  element "html" ? do
    fontSize $ px 12.0

  element "body" ? do
    fontSize $ rem 2.2
    fontFamily
      [ "Lora" ]
      $ NE.NonEmpty
          (GenericFontFamily $ fromString "Times")
          [ GenericFontFamily $ fromString "Serif" ]
    backgroundColor $ fromInt 0x282c34
    color nearlyWhite
    display grid
    gap $ rem 2.0
    gridTemplateRows $ tuple2 (rem 6.0) (auto :: Size _)

  typography

  element "pre" ? do
    margin (rem 2.0) nil (rem 2.0) nil
    border solid (px 1.0) nearlyWhite
    overflow overflowAuto

  element "pre" & byClass "wrap" ? do
    textWhitespace whitespacePreWrap

  element "code" ? do
    display inlineBlock

  element "pre" |* element "code" ? do
    margin (rem 2.0) (rem 2.0) (rem 2.0) (rem 2.0)

  element "p" |* element "code" ? do
    padding (rem 0.3) (rem 0.6) (rem 0.3) (rem 0.6)
    backgroundColor $ fromInt 0x1f232d

  element "img" ? do
    display block
    maxWidth $ pct 100.0
    margin nil auto nil auto

  Selector (Refinement [])
    (Combined (element "a") (element "a" & pseudo "hover")) ? do
    color nearlyWhite

  element "ul" ? do
    listStyleType disc
    paddingLeft $ rem 4.0

  element "ol" ? do
    listStyleType decimal
    paddingLeft $ rem 4.0

  element "li" ? do
    margin (rem 1.0) nil (rem 1.0) nil

  element "header" ? do
    gap $ rem 1.6
    display flex
    alignItems center
    padding nil (rem 5.0) nil (rem 5.0)
    backgroundColor white
    color black
    fontFamily
      [ "Helvetica" ]
      $ NE.NonEmpty
          (GenericFontFamily $ fromString "sans-serif")
          []

  element "header" |* element "a" ? do
    color $ fromInt 0x333333

  star & byClass "logo" ? do
    fontWeight bold
    textTransform uppercase
    letterSpacing $ px 3.0
    fontSize $ rem 2.0
    textDecoration noneTextDecoration

  (star & byClass "logo") & hover ? do
    color black

  star & byClass "logo-subtitle" ? do
    fontSize $ rem 1.8
    fontWeight lighter

  star & byClass "main-content" ? do
    padding nil (rem 2.0) nil (rem 2.0)
    overflow OverFlow.hidden

  star & byClass "post-date" ? do
    color $ fromInt 0xaaaaaa

  star & byClass "posts-index" ? do
    display flex
    flexDirection column

  star & byClass "post-entry" ? do
    pure unit

  queryLarge do
    element "html" ? do
      fontSize $ px 12.0

    star & byClass "main-content" ? do
      maxWidth $ px 900.0
      margin nil auto nil auto

