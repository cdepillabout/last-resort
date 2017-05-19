module LastResort.Config where

type Config =
  { title :: String
  , public_path :: String
  }

foreign import config :: Config
