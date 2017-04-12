
module LastResort.Logger where

import LastResort.Prelude

import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
       (logStdoutDev, logStdout)

import LastResort.Environment (Environment(..), HasEnvironment(getEnvironment))

requestLoggerMiddleware :: HasEnvironment r => r -> Middleware
requestLoggerMiddleware r =
  case getEnvironment r of
    Testing -> id
    Development -> logStdoutDev
    Production -> logStdout
