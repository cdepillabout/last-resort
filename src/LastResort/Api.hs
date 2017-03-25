module LastResort.Api
  ( defaultMainApi
  ) where

import LastResort.Prelude hiding (Handler)

import Control.Natural ((:~>)(NT))
import Network.Wai (Application, Middleware, Request)
import Network.Wai.Handler.Warp (run)
import Servant
       ((:>), (:<|>)(..), Context(..), Get, Handler, JSON, Post,
        ServantErr, Server, ServerT, enter, serve)
import Servant.Server.Experimental.Auth (AuthHandler)

import LastResult.Config (Config(..))

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

type ApiSearch = Post '[JSON] Int

type ApiStatus = Get '[JSON] Int

serverRoot :: {- (MonadLogger m) => -} ServerT Api Handler
serverRoot = search :<|> status

search :: Handler Int
search = pure 1

status :: Handler Int
status = pure 1

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Config -> Application
app config = serve (Proxy :: Proxy Api) $ apiServer config

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Config -> Server Api
apiServer config = enter natTrans serverRoot
  where
    natTrans :: LastResortM :~> Handler
    natTrans = NT trans

    trans :: forall a. LastResortM a -> Handler a
    trans = id
