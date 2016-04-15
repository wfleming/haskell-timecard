module Summary (
  summarizeEntries,
  showSummary,
  showSummaryCSV
) where

import Data.List (find, nub)
import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import Data.Time.LocalTime (localDay, zonedTimeToLocalTime)
import Entry
import EntryLog
import Text.Printf

type ProjectHours = (String, Double)
type DaySummary = (Day, [ProjectHours])
type Summary = [DaySummary]

summarizeEntries :: EntryLog -> Summary
summarizeEntries = foldl insertEntryIntoSummary []

insertEntryIntoSummary :: Summary -> Entry -> Summary
insertEntryIntoSummary [] e = [summarizeEntryForDay (entryDay e, []) e]
insertEntryIntoSummary s e | fst (last s) == entryDay e = init s ++ [ds']
  where
    ds = last s
    ds' = summarizeEntryForDay ds e
insertEntryIntoSummary s e = s ++ insertEntryIntoSummary [] e

summarizeEntryForDay :: DaySummary -> Entry -> DaySummary
summarizeEntryForDay (d, phs) e = (d, addOrAppend phs)
  where
    addOrAppend :: [ProjectHours] -> [ProjectHours]
    addOrAppend [] = [(projectName e, duration e)]
    addOrAppend ((p, hrs) : t) | p == projectName e = (p, hrs + duration e) : t
    addOrAppend (h : t) = h : addOrAppend t

entryDay :: Entry -> Day
entryDay = localDay . zonedTimeToLocalTime . timeIn

showSummary :: Summary -> String
showSummary s = unlines (map showDay s)
  where
    showDay (d, phs) = unlines ((dateLine d : map projectLine phs) ++ [dayTotalLine phs])
    dateLine = formatTime defaultTimeLocale "%A, %d %B %Y"
    projectLine (p, h) = printf "\t%s:\t%.2f" p h
    dayTotalLine phs = printf "\tDAY TOTAL: %.2f" (sum $ map snd phs)

{- The CSV output:
 Project, <YYYY-MM-DD>, <YYYY-MM-DD>, ...
 <name>, <hours>, <hours>, ...
-}
showSummaryCSV :: Summary -> String
showSummaryCSV s = unlines (headerLine : projectLines)
  where
  dates = map fst s
  projects = nub $ concatMap (\(_, phs) -> map fst phs) s
  headerLine = "Project," ++ joinComma (map csvDate dates)
  csvDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  projectLines = map projectLine projects
  projectLine p = joinComma (p : map (printf "%.2f") (projectLineHours p))
  projectLineHours :: String -> [Hours]
  projectLineHours p = map pHoursForDate dates
    where
      pHoursForDate :: Day -> Hours
      pHoursForDate d = pHoursForDate' d (dayAllPHs d)
      dayAllPHs d = case find (\(d', _) -> d' == d) s of
        Nothing -> []
        Just (_, phs) -> phs
      pHoursForDate' _ [] = 0.0
      pHoursForDate' _ ((p', hrs) : _) | p' == p = hrs
      pHoursForDate' d (_ : t) = pHoursForDate' d t
  joinComma [] = ""
  joinComma [h] = h
  joinComma (h : t) = h ++ "," ++ joinComma t
