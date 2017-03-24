{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Twitter
  where

import Prelude hiding (undefined)

import Control.Lens ((^?))
import Control.Monad.Catch (MonadCatch, handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON(..), Value, (.:), decode, withObject)
import Data.Aeson.Types (Parser)
import Data.Aeson.Lens (_String, key)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Base64 (encode)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Simple
       (HttpException, Request, Response, addRequestHeader,
        defaultRequest, getResponseBody, httpJSON, httpLBS, parseRequest,
        setRequestBodyLBS, setRequestHeaders, setRequestHost,
        setRequestMethod, setRequestPath, setRequestPort,
        setRequestQueryString, setRequestSecure)
import Network.HTTP.Types.URI (urlEncode)
import System.ReadEnvVar (lookupEnv)
import Web.Twitter.Types (SearchResult, Status)

-- import Text.Pretty.Simple


newtype ConsumerKey = ConsumerKey
  { unConsumerKey :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

newtype ConsumerSecret = ConsumerSecret
  { unConsumerSecret :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

newtype Credentials = Credentials
  { unCredentials :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

newtype BearerToken = BearerToken
  { unBearerToken :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

instance FromJSON BearerToken where
  parseJSON :: Value -> Parser BearerToken
  parseJSON = withObject "BearerToken" $ \obj -> do
    tokenType <- obj .: "token_type" :: Parser Text
    token <- obj .: "access_token"
    case tokenType of
      "bearer" -> pure . BearerToken $ encodeUtf8 token
      _ -> fail "BearerToken's \"token_type\" is not \"bearer\"."

class HasBearerToken r where
  getBearerToken :: r -> BearerToken

instance HasBearerToken BearerToken where
  getBearerToken :: BearerToken -> BearerToken
  getBearerToken = id

credentialsFromEnvVar :: MonadIO m => m (Maybe Credentials)
credentialsFromEnvVar = do
  consumerKey <- lookupEnv "TWITTER_CONSUMER_KEY"
  consumerSecret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  pure $ createOAuth2Creds <$> consumerKey <*> consumerSecret

createOAuth2Creds :: ConsumerKey -> ConsumerSecret -> Credentials
createOAuth2Creds (ConsumerKey consumerKey) (ConsumerSecret consumerSecret) =
  let urlEncodedConsumerKey = urlEncode False consumerKey
      urlEncodedConsumerSecret = urlEncode False consumerSecret
      credentials = urlEncodedConsumerKey <> ":" <> urlEncodedConsumerSecret
      b64Credentials = encode credentials
  in Credentials b64Credentials

createOAuth2TokenReq :: Credentials -> Request
createOAuth2TokenReq (Credentials credentials) =
  addRequestHeader "Authorization" ("Basic " <> credentials) .
  addRequestHeader
    "Content-Type"
    "application/x-www-form-urlencoded;charset=UTF-8" .
  setRequestBodyLBS "grant_type=client_credentials" .
  setRequestHost "api.twitter.com" .
  setRequestMethod "POST" .
  setRequestPath "oauth2/token" .
  setRequestPort 443 .
  setRequestSecure True $
  defaultRequest

respToBearerToken :: Response LBS.ByteString -> Either TwitterError BearerToken
respToBearerToken resp =
  let body = getResponseBody resp
  in case decode body of
       Nothing -> Left ()
       Just ret -> Right ret

bearerTokenFromCreds
  :: forall m.
     (MonadCatch m, MonadIO m)
  => Credentials -> m (Either TwitterError BearerToken)
bearerTokenFromCreds creds = do
  let req = createOAuth2TokenReq creds
  liftIO $ print req
  handle err $ respToBearerToken <$> httpLBS req
  where
    err :: HttpException -> m (Either TwitterError BearerToken)
    err _ = pure $ Left ()

type TwitterError = ()

twitter
  :: (HasBearerToken r, FromJSON (TwitterReturn a), MonadIO m)
  => r -> TwitterRequest a -> m (Either TwitterError (TwitterReturn a))
twitter hasBearerToken TwitterRequest{method, endpoint, queryParams} = do
  let (BearerToken bearerToken) = getBearerToken hasBearerToken
      initReq =
        addRequestHeader "Authorization" ("Bearer " <> bearerToken) .
        setRequestHost "api.twitter.com" .
        setRequestMethod (methodToByteString method) .
        setRequestPath ("1.1/" <> encodeUtf8 endpoint) .
        setRequestPort 443 .
        setRequestSecure True $
        defaultRequest
      req =
        case method of
          GET -> setRequestQueryString (fmap Just <$> queryParams) initReq
  resp <- httpLBS req
  case decode $ getResponseBody resp of
    Nothing -> pure $ Left ()
    Just ret -> pure $ Right ret

newtype Param k v = Param
  { unParam :: (k, v)
  } deriving (Data, Eq, Read, Show, Typeable)

type Params = [(ByteString, ByteString)]

data Method
  = DELETE
  | GET
  | POST
  deriving (Data, Eq, Read, Show, Typeable)

methodToByteString :: Method -> ByteString
methodToByteString = B8.pack . show

data TwitterRequest a = TwitterRequest
  { method :: Method
  , endpoint :: Text
  , queryParams :: Params
  } deriving (Data, Eq, Read, Show, Typeable)

class ToTwitterParam param where
  toTwitterParam :: param -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]

class ToTwitterParam param => TwitterHasParam request param

type family TwitterReturn a :: *

mkTwitterRequest :: Method -> Text -> Params -> TwitterRequest a
mkTwitterRequest = TwitterRequest

(-&-)
  :: TwitterHasParam request param
  => TwitterRequest request -> param -> TwitterRequest request
twitReq -&- param =
  twitReq {queryParams = toTwitterParam param (queryParams twitReq)}

newtype SearchString = SearchString
  { unSearchString :: Text
  } deriving (Data, Eq, IsString, Read, Show, Typeable)

newtype Count = Count
  { unCount :: Int
  } deriving (Data, Eq, Num, Read, Show, Typeable)

instance ToTwitterParam Count where
  toTwitterParam :: Count
                 -> [(ByteString, ByteString)]
                 -> [(ByteString, ByteString)]
  toTwitterParam (Count count) =
    (("count", B8.pack $ show count) :)

instance ToTwitterParam SearchString where
  toTwitterParam :: SearchString
                 -> [(ByteString, ByteString)]
                 -> [(ByteString, ByteString)]
  toTwitterParam (SearchString query) =
    (("q", encodeUtf8 query) :)

data SearchTweets

type instance TwitterReturn SearchTweets = SearchResult [Status]

instance TwitterHasParam SearchTweets Count

searchTweets :: SearchString -> TwitterRequest SearchTweets
searchTweets searchString =
  mkTwitterRequest GET "search/tweets.json" $ toTwitterParam searchString []
