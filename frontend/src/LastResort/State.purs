module LastResort.State where

import LastResort.Prelude

import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)

import LastResort.Config (config)
import LastResort.Routes (Route, match)

data Input a
  = PreInput
  | Input a

derive instance genericInput :: Generic (Input a) _
instance showInput :: Show a => Show (Input a) where show = genericShow
instance decodeInput :: Decode a => Decode (Input a) where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInput :: Encode a => Encode (Input a) where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

newtype State = State
  { loaded :: Boolean
  , route :: Route
  , searchString :: Input String
  , title :: String
  }

derive instance genericState :: Generic State _
derive instance newtypeState :: Newtype State _

instance showState :: Show State where show = genericShow

init :: String -> State
init url = State
  { loaded: false
  , route: match url
  , searchString: PreInput
  , title: config.title
  }
