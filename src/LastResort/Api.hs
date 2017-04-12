module LastResort.Api
  ( defaultMainApi
  ) where

import LastResort.Prelude hiding (Handler)

import Control.Natural ((:~>)(NT))
import Network.Wai (Application, Middleware, Request)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Servant
       ((:>), (:<|>)(..), Context(..), Get, Handler, JSON, Post,
        ServantErr, Server, ServerT, enter, serve)
import Servant.Server.Experimental.Auth (AuthHandler)
import Web.Twitter.AppOnly
       (Count(Count), Status, SearchResult, (-&-), twitter, searchTweets)

import LastResort.Config
       (Config(..), Environment(..), HasEnvironment(getEnvironment),
        configFromEnv)

requestLoggerMiddleware :: (HasEnvironment r, MonadReader r m) => m Middleware
requestLoggerMiddleware = do
  r <- ask
  pure $
    case getEnvironment r of
      Testing -> id
      Development -> logStdoutDev
      Production -> logStdout

setup :: IO (Config, Middleware)
setup = do
  config <- configFromEnv
  let loggerMiddleware = requestLoggerMiddleware config
  return (config, loggerMiddleware)

defaultMainApi :: IO ()
defaultMainApi = do
  (config, loggerMiddleware) <- setup
  let port = configPort config
  putStrLn $ "last-resort running on port " <> tshow (configPort config)
  run port . loggerMiddleware $ app config

type LastResortM = Handler

type Api = "v0" :> (ApiSearch :<|> ApiStatus)

type ApiSearch = "search" :> Post '[JSON] (SearchResult [Status])

type ApiStatus = "status" :> Get '[JSON] Int

serverRoot :: {- (MonadLogger m) => -} Config -> ServerT Api Handler
serverRoot config = search config :<|> status

search :: Config -> Handler (SearchResult [Status])
search config = do
  eitherStatuses <- twitter config $ searchTweets "ヤギ" -&- Count 10
  case eitherStatuses of
    Left twitterErr -> undefined
    Right statuses -> pure statuses

status :: Handler Int
status = pure 1

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> Application
app config = serve (Proxy :: Proxy Api) $ apiServer config

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Config -> Server Api
apiServer config = enter natTrans (serverRoot config)
  where
    natTrans :: LastResortM :~> Handler
    natTrans = NT trans

    trans :: forall a. LastResortM a -> Handler a
    trans = id
