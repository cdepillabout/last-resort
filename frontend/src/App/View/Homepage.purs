module App.View.Homepage where

import App.Events (Event)
import App.State (State)
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), text)

view :: State -> HTML Event
view s =
  div do
    h1 $ text "Pux"
    h1 $ text "Yo New thing"
    h1 $ text "Yo New thing 2"
    h1 $ text "Yo New thing 3"
    h1 $ text "Yo New thing 4"
    h1 $ text "Yo New thing 5"
    h1 $ text "Yo New thing 6"
    a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
