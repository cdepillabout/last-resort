module LastResort.State where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Show (class Show)

import LastResort.Config (config)
import LastResort.Routes (Route, match)

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
