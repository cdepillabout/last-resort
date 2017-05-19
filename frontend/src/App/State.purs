module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

newtype State = State
  { loaded :: Boolean
  , route :: Route
  , searchString :: String
  , title :: String
  }

derive instance genericState :: Generic State _
derive instance newtypeState :: Newtype State _

instance showState :: Show State where show = genericShow

init :: String -> State
init url = State
  { loaded: false
  , route: match url
  , searchString: ""
  , title: config.title
  }
