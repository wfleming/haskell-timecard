module EntryLog where

import Entry
import Data.Time.LocalTime (ZonedTime)

type EntryLog = [Entry]

lastEntry :: EntryLog -> Maybe Entry
lastEntry [] = Nothing
lastEntry el = Just (last el)

punchIn :: ZonedTime -> EntryLog -> String -> EntryLog
punchIn now el project' = el ++ [newEntry]
  where
    newEntry = case lastEntry el of
      Nothing -> Entry project' now Nothing
      Just (Entry _ _ (Just _)) -> Entry project' now Nothing
      Just (Entry _ _ Nothing) -> error "You need to punch out first."

punchOut :: ZonedTime -> EntryLog -> EntryLog
punchOut now el = init el ++ [modifiedEntry]
  where
    modifiedEntry = case lastEntry el of
      Just (Entry project' timeIn' Nothing) -> Entry project' timeIn' (Just now)
      Nothing -> error "You need to punch in first."
      Just (Entry _ _ (Just _)) -> error "You need to punch in first."
