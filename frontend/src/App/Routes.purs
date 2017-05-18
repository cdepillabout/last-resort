module App.Routes where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Pux.Router (end, lit, router)

data Route
  = Home
  | NotFound String
  | SearchResults

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where show = genericShow
instance decodeRoute :: Decode Route where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoute :: Encode Route where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end <|>
  SearchResults <$ lit "search" <* end

toUrl :: Route -> String
toUrl (Home) = "/"
toUrl (NotFound url) = url
toUrl (SearchResults) = "/search"

titleForRoute :: Route -> String
titleForRoute Home = "Last Resort"
titleForRoute (NotFound _) = prependLastResort "404 Not Found"
titleForRoute SearchResults = prependLastResort "Search Results"

prependLastResort :: String -> String
prependLastResort = append "Last Resort | "
