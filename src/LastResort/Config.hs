
module LastResort.Config where

import LastResort.Prelude

import Network.Wai.Handler.Warp (Port)
import System.ReadEnvVar (readEnvDef)
import Twitter
       (BearerToken, Credentials, HasBearerToken(..), credentialsFromEnv)

data Environment
  = Development
  | Testing
  | Production
  deriving (Data, Eq, Ord, Read, Show, Typeable)

class HasEnvironment r where
  getEnvironment :: r -> Environment

instance HasEnvironment Environment where
  getEnvironment :: Environment -> Environment
  getEnvironment = id

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

configFromEnv :: MonadIO m => m Config
configFromEnv = do
  env <- readEnvDef "ENV" Development
  port <- readEnvDef "PORT" 8105
  maybeCreds <- credentialsFromEnv
  createConfig creds env port

createConfig :: MonadIO m => Credentials -> Environment -> Port -> m Config
createConfig creds env port = do
  bearerToken <- bearerTokenFromCreds creds
  pure $ Config bearerToken env port
