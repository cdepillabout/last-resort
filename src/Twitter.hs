{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitter
  where

import Prelude

import Control.Lens ((^?))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (Value)
import Data.Aeson.Lens (_String, key)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (encode)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Simple
       (Request, addRequestHeader, defaultRequest, getResponseBody,
        httpJSON, httpLBS, parseRequest, setRequestBodyLBS,
        setRequestHeaders, setRequestHost, setRequestMethod,
        setRequestPath, setRequestSecure)
import Network.HTTP.Types.URI (urlEncode)
import System.ReadEnvVar (lookupEnv)

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
  addRequestHeader
    "Content-Type"
    "application/x-www-form-urlencoded;charset=UTF-8" .
  addRequestHeader "Authorization" ("Basic " <> credentials) .
  setRequestBodyLBS "grant_type=client_credentials" .
  setRequestPath "oauth2/token" .
  setRequestSecure True .
  setRequestHost "api.twitter.com" . setRequestMethod "POST" $
  defaultRequest


-- what :: IO ()
-- what = do
--   consumerKey <- lookupEnvEx "TWITTER_CONSUMER_KEY"
--   consumerSecret <- lookupEnvEx "TWITTER_CONSUMER_SECRET"
--   let urlEncodedConsumerKey = urlEncode False consumerKey
--       urlEncodedConsumerSecret = urlEncode False consumerSecret
--       credentials = urlEncodedConsumerKey <> ":" <> urlEncodedConsumerSecret
--       b64Credentials = encode credentials
--   -- print b64Credentials
--   initReq <- parseRequest "POST https://api.twitter.com/oauth2/token"
--   let headers =
--         [ ("Authorization", "Basic " <> b64Credentials)
--         , ("Content-Type", "application/x-www-form-urlencoded;charset=UTF-8")
--         ]
--       req =
--         setRequestHeaders headers .
--         setRequestBodyLBS "grant_type=client_credentials" $
--         initReq
--   resp <- httpLBS req
--   -- pPrint resp
--   let body = getResponseBody resp
--       maybeTokenType = body ^? key "token_type" . _String
--       maybeToken = body ^? key "access_token" . _String
--   pPrint maybeTokenType
--   pPrint maybeToken

--   initReq2 <- parseRequest "https://api.twitter.com/1.1/statuses/user_timeline.json?count=10&screen_name=twitterapi"
--   let headers2 = [("Authorization", "Bearer " <> encodeUtf8 (fromMaybe undefined maybeToken))]
--       req2 = setRequestHeaders headers2 initReq2
--   resp2 <- httpJSON req2
--   pPrint $ (getResponseBody resp2 :: Value)
