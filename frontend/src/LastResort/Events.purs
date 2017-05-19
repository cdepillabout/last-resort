module LastResort.Events where

import LastResort.Prelude

import Control.Monad.Aff (Aff)
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
import Pux.DOM.Events (DOMEvent, targetValue)

import LastResort.Routes (Route(..), SearchParams(..), match, titleForRoute, toUrl)
import LastResort.State (Input(Input), State(..), stateToSearchParams)

data Event
  = Navigate String DOMEvent
  | PageView Route
  | Search DOMEvent
  | SearchStringChange DOMEvent

type AppEffects fx = (ajax :: AJAX, dom :: DOM, history :: HISTORY | fx)

foldp :: forall fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) state = foldPageView route state
foldp (Navigate url domEvent) state = foldNavigate url domEvent state
foldp (Search domEvent) state = foldSearch domEvent state
foldp (SearchStringChange domEvent) state =
  foldSearchStringChanged domEvent state

foldPageView
  :: forall fx.
     Route -> State -> EffModel State Event (AppEffects fx)
foldPageView route state =
  { effects: effectsForPageView route state
  , state: updateRouteForPageView route state
  }

updateRouteForPageView :: Route -> State -> State
updateRouteForPageView route (State state) =
    State
      state
        { loaded = true
        , route = route
        , searchString =
            case route of
              (SearchResults (SearchParams searchParams)) ->
                Input searchParams.query
              _ -> state.searchString
        , title = titleForRoute route
        }

effectsForPageView
  :: forall fx.
     Route -> State -> Array (Aff (AppEffects fx) (Maybe Event))
effectsForPageView (SearchResults (SearchParams searchParams)) state =
  [
  ]
effectsForPageView route state = []

foldNavigate
  :: forall fx.
     String -> DOMEvent -> State -> EffModel State Event (AppEffects fx)
foldNavigate url domEvent state =
  onlyEffects
    state
    [ liftEff do
        preventDefault domEvent
        hist <- history =<< window
        pushState
          (toForeign {})
          (DocumentTitle <<< titleForRoute $ match url)
          (URL url)
          hist
        pure $ Just $ PageView (match url)
    ]

foldSearchStringChanged
  :: forall fx ev.
     DOMEvent -> State -> EffModel State ev fx
foldSearchStringChanged domEvent (State state) =
  noEffects $ State state { searchString = Input $ targetValue domEvent }

foldSearch
  :: forall fx.
     DOMEvent -> State -> EffModel State Event (AppEffects fx)
foldSearch domEvent state =
  onlyEffects
    state
    [ do
        liftEff $preventDefault domEvent
        pure $ Just $ Navigate (toUrl (SearchResults (stateToSearchParams state))) domEvent
    ]

