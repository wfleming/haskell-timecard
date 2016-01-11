{-# LANGUAGE OverloadedStrings #-}
module Entry (Entry(..), Hours, show, duration, parseEntry) where

import Data.Time.Clock (diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)

data Entry = Entry { projectName :: String
                   , timeIn :: ZonedTime
                   , timeOut :: Maybe ZonedTime
                   }
type Hours = Double

parseEntry :: String -> Entry
parseEntry str =
  let
    -- basically copied from base words, but \t instead of \n
    split :: String -> [String]
    split s = case dropWhile (== '\t') s of
      "" -> []
      s' -> w : words s''
        where (w, s'') = break (== '\t') s'
    pieces = split str
    parsedName :: Maybe String
    parsedName = case pieces of
      n  : _ -> Just n
      _ -> Nothing
    parsedTimeIn :: Maybe ZonedTime
    parsedTimeIn = case pieces of
      _ : t : _ -> timeFromString t
      _ -> Nothing
    parsedTimeOut :: Maybe ZonedTime
    parsedTimeOut = case pieces of
      [_, _, t] -> timeFromString t
      _ -> Nothing
  in
    case (parsedName, parsedTimeIn, parsedTimeOut) of
      (Just name, Just timeIn', timeOut') -> Entry name timeIn' timeOut'
      foo -> error ("Couldn't parse entry from '" ++ str ++ "'  :: " ++ show foo)


instance Show Entry where
  show e =
    case timeOut e of
      Just t -> projectName e ++ "\t" ++ showTime (timeIn e) ++ "\t" ++ showTime t ++ "\n"
      Nothing -> projectName e ++ "\t" ++ showTime (timeIn e)

-- limitations with time handling: package docs say it can parse %z for -HH:MM,  but it *formats*
-- it as -HHMM, which isn't valid iso8601: so we use it for parsing (which also unfortunately
-- means no implementation is allowed to write Z for UTC), but dance around it for formatting.

timeFromString :: String -> Maybe ZonedTime
timeFromString = parseTimeM True defaultTimeLocale "%FT%T%z"

showTime :: ZonedTime -> String
showTime t = dateAndTime ++ tz
  where
    dateAndTime = formatTime defaultTimeLocale "%FT%T" t
    (h, m) = splitAt 3 (formatTime defaultTimeLocale "%z" t)
    tz = h ++ ":" ++ m

duration :: Entry -> Hours
duration e =
  case timeOut e of
    Just t -> roundedDouble (durationSec t) / secInHour
    Nothing -> 0
  where
    durationSec tOut = diffUTCTime (zonedTimeToUTC tOut) (zonedTimeToUTC (timeIn e))
    roundedDouble d = fromInteger (round d) :: Double
    secInHour = (60.0 * 60.0) :: Double

