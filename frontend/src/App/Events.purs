module App.Events where

import Prelude

import App.Routes (Route, match, titleForRoute)
import App.State (State(..))
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(DocumentTitle), URL(URL), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent)

data Event
  = Navigate String DOMEvent
  | PageView Route

type AppEffects fx = (ajax :: AJAX, dom :: DOM, history :: HISTORY | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State state) =
  noEffects $
    State
      state
        { route = route
        , loaded = true
        , title = titleForRoute route
        }
foldp (Navigate url event) state =
  onlyEffects
    state
    [ liftEff do
        preventDefault event
        hist <- history =<< window
        pushState
          (toForeign {})
          (DocumentTitle <<< titleForRoute $ match url)
          (URL url)
          hist
        pure $ Just $ PageView (match url)
    ]
