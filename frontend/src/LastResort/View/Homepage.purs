module LastResort.View.Homepage where

import LastResort.Prelude hiding (div)

import Pux.DOM.Events (onChange, onClick, onSubmit)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, form, h1, input, nav, li, ul)
import Text.Smolder.HTML.Attributes (href, name, type', value)
import Text.Smolder.Markup ((!), (#!), text)

import LastResort.Events (Event(..))
import LastResort.State (State(..))

view :: State -> HTML Event
view (State state) =
  div do
    h1 $ text "Pux"
    -- h1 $ text "nothing"
    -- a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    -- a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
    form ! name "search" #! onSubmit (const Search) $ do
      input ! type' "text" ! value state.searchString #! onChange SearchStringChange


-- TODO: This shows an example of using onClick to send application Navigation events.
navigation :: HTML Event
navigation =
    nav do
      ul do
        li $ a ! href "/" #! onClick (Navigate "/") $ text "Home"
        li $ a ! href "/users" #! onClick (Navigate "/users") $ text "Users"
        li $ a ! href "/users?sortBy=age" #! onClick (Navigate "/users?sortBy=age") $ text "Users sorted by age."
        li $ a ! href "/users/123" #! onClick (Navigate "/users/123") $ text "User 123"
        li $ a ! href "/foobar" #! onClick (Navigate "/foobar") $ text "Not found"
