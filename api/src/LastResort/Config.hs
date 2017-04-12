
module LastResort.Config where

import LastResort.Prelude

import Control.FromSum (fromEitherOrM, fromMaybeOrM)
import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (readEnvDef)
import Web.Twitter.AppOnly.Auth
       (BearerToken, Credentials, HasBearerToken(..), TwitterError,
        bearerTokenFromCreds, credentialsFromEnv)

import LastResort.Environment (Environment(..), HasEnvironment(..))

data Config = Config
  { configBearer :: BearerToken
  , configEnvironment :: Environment
  , configPort :: Port
  } deriving (Data, Eq, Read, Show, Typeable)

instance HasEnvironment Config where
  getEnvironment :: Config -> Environment
  getEnvironment = configEnvironment

instance HasBearerToken Config where
  getBearerToken :: Config -> BearerToken
  getBearerToken = configBearer

configFromEnv :: (MonadCatch m, MonadIO m) => m Config
configFromEnv = do
  env <- readEnvDef "ENV" Development
  port <- readEnvDef "PORT" 8105
  maybeCreds <- credentialsFromEnv
  creds <-
    fromMaybeOrM maybeCreds . fail $
      "Required env vars \"TWITTER_CONSUMER_KEY\" and " <>
      "\"TWITTER_CONSUMER_SECRET\" not found."
  eitherConfig <- createConfig creds env port
  fromEitherOrM eitherConfig $ \configErr ->
    fail $
      "Following error occurred when trying to create the Config:\n" <>
      show configErr

data ConfigError =
  CouldNotCreateBearerToken TwitterError
  deriving (Data, Eq, Read, Show, Typeable)

createConfig
  :: (MonadCatch m, MonadIO m)
  => Credentials -> Environment -> Port -> m (Either ConfigError Config)
createConfig creds env port = do
  eitherBearerToken <- bearerTokenFromCreds creds
  pure $
    case eitherBearerToken of
      Left twitterError -> Left $ CouldNotCreateBearerToken twitterError
      Right bearerToken -> Right $ Config bearerToken env port
