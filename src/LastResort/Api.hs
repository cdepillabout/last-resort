module LastResort.Api
  ( defaultMainApi
  ) where

import LastResort.Prelude

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)

-- import LastResort.Config
--        (Config(..), createConfigFromEnvVars, getRequestLoggerMiddleware)
-- import LastResort.Db (doMigrations)
-- import LastResort.Handler (app)

-- setup :: IO (Config, Middleware)
-- setup = do
--   cfg <- createConfigFromEnvVars
--   let requestLoggerMiddleware = getRequestLoggerMiddleware $ configEnv cfg
--   return (cfg, requestLoggerMiddleware)

defaultMainApi :: IO ()
defaultMainApi = undefined -- do
  -- (cfg, requestLoggerMiddleware) <- setup
  -- let port = configPort cfg
  -- putStrLn $ "last-resort running on port " <> tshow port <> "..."
  -- run port . requestLoggerMiddleware $ app cfg



-- {-# LANGUAGE OverloadedStrings #-}

-- module Lib where

-- import Control.Lens ((^?))
-- import Data.Aeson (Value)
-- import Data.Aeson.Lens (_String, key)
-- import Data.ByteString.Base64 (encode)
-- import Data.Maybe (fromMaybe)
-- import Data.Monoid ((<>))
-- import Data.Text.Encoding (encodeUtf8)
-- import Network.HTTP.Simple
--        (getResponseBody, httpJSON, httpLBS, parseRequest,
--         setRequestBodyLBS, setRequestHeaders)
-- import Network.HTTP.Types.URI (urlEncode)
-- import System.ReadEnvVar (lookupEnvEx)

-- import Text.Pretty.Simple

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"


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
