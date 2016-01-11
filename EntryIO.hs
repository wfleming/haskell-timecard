module EntryIO(EntryIO, entries, outH, openEntryIO, punchInIO, punchOutIO) where

import Data.Time.LocalTime (getZonedTime, ZonedTime)
import EntryLog
import Entry
import System.IO

data EntryIO = MakeEntryIO { entries :: IO EntryLog, outH :: Handle }

openEntryIO :: Handle -> Handle -> EntryIO
openEntryIO inHandle = MakeEntryIO (allEntries inHandle)

allEntries :: Handle -> IO EntryLog
allEntries h = allEntries' h []
  where
    allEntries' h' es = do
      eof <- hIsEOF h'
      nextEntries h' eof es
    nextEntries _ True entries' = return entries'
    nextEntries h'' False entries' = do
        e <- hGetLine h''
        allEntries' h'' (entries' ++ [parseEntry e])

withCurrentTime :: (ZonedTime -> a -> b) -> IO (a -> b)
withCurrentTime f = do
  now <- getZonedTime
  return (f now)

punchInIO :: EntryIO -> String -> IO ()
punchInIO eio project = do
  es <- doPunch <*> entries eio <*> pure project
  hSeek (outH eio)  SeekFromEnd 0
  printEntry (outH eio) (lastEntry es)
  return ()
  where
    doPunch = withCurrentTime punchIn
    printEntry h (Just entry) = hPutStr h (show entry)
    printEntry _ Nothing = error "Cannot print Nothing -- need an entry."

punchOutIO :: EntryIO -> IO ()
punchOutIO eio = do
  esBefore <- entries eio
  esAfter <- doPunchM <*> entries eio
  hSeek (outH eio)  SeekFromEnd 0
  hPutStr (outH eio) (entryDiff (lastEntry esBefore) (lastEntry esAfter))
  return ()
  where
    doPunchM = withCurrentTime punchOut
    entryDiff :: Maybe Entry -> Maybe Entry -> String
    entryDiff (Just e1) (Just e2) = drop (length (show e1)) (show e2)
    entryDiff Nothing _ = error "Can't diff Nothing with Entry."
    entryDiff _ Nothing = error "Can't diff Nothing with Entry."
