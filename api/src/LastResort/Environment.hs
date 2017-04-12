
module LastResort.Environment where

import LastResort.Prelude

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
