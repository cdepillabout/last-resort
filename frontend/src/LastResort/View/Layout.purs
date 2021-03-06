module LastResort.View.Layout where

import LastResort.Prelude hiding (div)

import CSS (CSS, fromString, (?), fontSize, display, inlineBlock, marginTop, marginRight, marginLeft, px, value, key, color, backgroundColor, padding, borderRadius)
import CSS.Border (border, solid)
import CSS.TextAlign (center, textAlign)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import Color (rgb)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

import LastResort.Events (Event)
import LastResort.Routes (Route(..))
import LastResort.State (State(..))
import LastResort.View.Homepage as Homepage
import LastResort.View.NotFound as NotFound
import LastResort.View.SearchResults as SearchResults

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css
    case st.route of
      Home -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)
      (SearchResults _) -> SearchResults.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250

  fromString "body" ? do
    backgroundColor (rgb 0 20 30)
    let fontFamily =
          "-apple-system," <>
          "BlinkMacSystemFont," <>
          "\"Segoe UI\"," <>
          "Roboto," <>
          "Oxygen-Sans," <>
          "Ubuntu," <>
          "Cantarell," <>
          "\"Helvetica Neue\"," <>
          "sans-serif"
    key (fromString "font-family") (value fontFamily)
    color white
    textAlign center

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString "a" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration

  fromString ".guide" ? do
    border solid (2.0 #px) green
    color green
    marginRight (10.0 #px)

  fromString ".guide:hover" ? do
    backgroundColor green
    color white

  fromString ".github" ? do
    border solid (2.0 #px) blue
    color blue
    marginLeft (10.0 #px)

  fromString ".github:hover" ? do
    backgroundColor blue
    color white
