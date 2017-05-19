module LastResort.Routes where

import LastResort.Prelude

import Control.Alt ((<|>))
import Data.Newtype (class Newtype)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (fromMaybe)
import Global (encodeURIComponent)
import Pux.Router (Match, end, lit, param, router)

newtype SearchParams
  = SearchParams { query :: String }

derive instance genericSearchParams :: Generic SearchParams _
derive instance newtypeSearchParams :: Newtype SearchParams _
instance showSearchParams :: Show SearchParams where show = genericShow
instance decodeSearchParams :: Decode SearchParams where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchParams :: Encode SearchParams where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

createSearchParams :: String -> SearchParams
createSearchParams query = SearchParams { query: query }

data Route
  = Home
  | NotFound String
  | SearchResults SearchParams

derive instance genericRoute :: Generic Route _
instance showRoute :: Show Route where show = genericShow
instance decodeRoute :: Decode Route where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoute :: Encode Route where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end <|>
  SearchResults <$> (lit "search" *> matchSearchParams) <* end

matchSearchParams :: Match SearchParams
matchSearchParams =
  createSearchParams <$> param "query"

toUrl :: Route -> String
toUrl (Home) = "/"
toUrl (NotFound url) = url
toUrl (SearchResults (SearchParams searchParams)) =
  "/search?query=" <> encodeURIComponent searchParams.query

titleForRoute :: Route -> String
titleForRoute Home = "Last Resort"
titleForRoute (NotFound _) = prependLastResort "404 Not Found"
titleForRoute (SearchResults _) = prependLastResort "Search Results"

prependLastResort :: String -> String
prependLastResort = append "Last Resort | "
