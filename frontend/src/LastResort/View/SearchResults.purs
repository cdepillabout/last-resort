module LastResort.View.SearchResults where

import LastResort.Prelude hiding (div)

import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), text)

import LastResort.Events (Event)
import LastResort.State (State)

view :: State -> HTML Event
view s =
  div do
    -- TODO: Add another search bar here to be able to repeat the searches from this page as well.
    h1 $ text "Search Results"
    -- a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    -- a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
