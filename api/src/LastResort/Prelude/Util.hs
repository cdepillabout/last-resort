{-# OPTIONS -fno-warn-orphans #-}

module LastResort.Prelude.Util where

import ClassyPrelude

import Data.Time.Format (parseTimeM)

-- | Double fmap.
--
-- >>> let foo = Just $ Just 3
-- >>> (+1) <$$> foo
-- Just (Just 4)
(<$$>)
    :: forall f g a b . (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixl 4 <$$>

-- | ISO Time format.
isoTimeFormat :: String
isoTimeFormat = "%Y-%m-%dT%H:%M:%S%QZ"

-- | Parse a time in ISO format.
--
-- >>> parseISOTime "2016-02-10T20:30:40.1234Z"
-- Just 2016-02-10 20:30:40.1234 UTC
-- >>> parseISOTime "foobar"
-- Nothing
parseISOTime :: String -> Maybe UTCTime
parseISOTime = parseTimeM True defaultTimeLocale isoTimeFormat

formatISOTime :: UTCTime -> String
formatISOTime = formatTime defaultTimeLocale isoTimeFormat

