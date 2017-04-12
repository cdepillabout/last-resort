
module LastResort.Logger where

import Network.Wai.Middleware.RequestLogger
       (logStdoutDev, logStdout)

import LastResort.Config (HasEnvironment(getEnvironment))

requestLoggerMiddleware :: HasEnvironment r => r -> Middleware
requestLoggerMiddleware r =
  case getEnvironment r of
    Test -> id
    Development -> logStdoutDev
    Production -> logStdout
