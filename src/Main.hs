{-# LANGUAGE OverloadedStrings #-}

module Main where

-- I prefer explicit import lists because it's easier to
-- find where types and functions are coming from without
-- needing a text editor with ghc-mod.

import Prelude hiding (putStrLn)

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON(..), (.:), decode, withObject)
import Network.HTTP.Conduit (simpleHttp)
import Data.Text (Text)
import Data.Text.IO (putStrLn)

type URL = String
type City = String
type Code = String
type Emoji = Text

data Weather = Weather { code :: String }

-- `withObject` is nice because it'll give slightly better
-- error message when the type isn't an `Object`.
--
-- Watch out for usage of `head` because it's partial function.
-- The error is prints out isn't very useful for figuring out
-- what the issue is:
--   *** Exception: Prelude.head: empty list
--

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \o -> do
    weatherValue <- head <$> o .: "weather"
    Weather <$> weatherValue .: "icon"

-- If I was already using the `lens` library, I would pull in
-- `lens-aeson` and write the instance to be total:
--
-- import Data.Text (Text, unpack)
-- import Control.Lens ((^.))
-- import Data.Aeson.Lens (_String, key, nth)
--
-- instance FromJSON Weather where
--   parseJSON v =
--     let maybeCode = v ^? key "weather" . nth 0 . key "icon" . _String
--     in case maybeCode of
--       Nothing -> return $ Weather "⁉️"
--       Just code' -> return $ Weather (unpack code')

apiUrl :: URL
apiUrl = "http://api.openweathermap.org/data/2.5/weather?q="

urlBuilder :: City -> URL
urlBuilder city = apiUrl <> city <> "&units=metric"

-- It's possible to avoid using do-notation here and only
-- using the `Application` instance of `IO`. YMMV on whether
-- you like this better.

getWeather :: City -> IO (Maybe Weather)
getWeather city = decode <$> (simpleHttp $ urlBuilder city)


-- `take` is a partial function. Added a length check beforehand
-- to make the function total
getEmoji :: Code -> Emoji
getEmoji input =
  if length input < 2
  then "⁉️"
  else case take 2 input of
    "01" -> "☀️" -- sun
    "02" -> "⛅️" -- sun with cloud
    "03" -> "☁️" -- cloud
    "04" -> "☁️" -- cloud
    "09" -> "💦" -- rain
    "10" -> "💦" -- rain
    "11" -> "⚡️" -- thunder
    "13" -> "❄️" -- snow
    "50" -> "♒︎" -- mist
    _ -> "⁉️"


-- `listToMaybe` takes the first element of a list if there is
-- one. Nice and easy way to make a list function total if you
-- only care about the first element.

-- Same as using only the `Applicative` instance of `IO` above.
parseArgs :: IO City
parseArgs = readCity <$> getArgs
  where readCity args = case listToMaybe args of
                          Nothing -> error "No City given."
                          Just s -> s

main :: IO ()
main = do
  city <- parseArgs
  response <- getWeather city
  case response of
    (Just w) -> putStrLn $ getEmoji $ code $ w
    Nothing  -> error "Failed to fetch weather info."
