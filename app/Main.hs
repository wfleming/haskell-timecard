module Main(main) where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Entry
import EntryIO
import EntryLog
import Summary
import System.Environment (getArgs, getEnv)
import System.IO

defaultLogFile :: IO FilePath
defaultLogFile = (++ "/.punch/entries.log") <$> getEnv "HOME"

main :: IO ()
main = do
  args <- getArgs
  case args of
    h : t -> command h t
    _ -> command "help" []

commands :: Map String ([String] -> IO ())
commands = Map.fromList [
    ("help", helpCommand),
    ("in", punchInCommand),
    ("out", punchOutCommand),
    ("summary", summaryCommand)
  ]

command :: String -> [String] -> IO ()
command str = Map.findWithDefault helpCommand str commands

logHandleIO :: IO Handle
logHandleIO = defaultLogFile >>= \path -> openFile path ReadWriteMode

useStdIn :: IO Bool
useStdIn = hReady stdin

entryIO :: IO EntryIO
entryIO = do
  h <- logHandleIO
  readStdin <- useStdIn
  return (openEntryIO (if readStdin then stdin else h) h)

helpCommand :: [String] -> IO ()
helpCommand ["in"] = putStrLn "usage: punch in <project name>"
helpCommand ["out"] = putStrLn "usage: punch out"
helpCommand ["summary"] = putStrLn "usage: punch summary [--csv]"
helpCommand _ = putStrLn ("Punch is a simple time tracker.\n\n" ++
  "Usage:\n\tpunch <command> [arguments]\n\n" ++
  "The commands are:\n" ++
  "\tin\tpunch in to a project\n" ++
  "\tout\tpunch out of a project\n" ++
  "\tsummary\tsummarize your time\n\n" ++
  "Use \"punch help [command]\" for more information about a command.")

punchInCommand :: [String] -> IO ()
punchInCommand [] = helpCommand ["in"]
punchInCommand args = do
  eio <- entryIO
  punchInIO eio project
  hClose (outH eio)
  putStrLn ("Punched in to " ++ project)
  where
    project = unwords args

punchOutCommand :: [String] -> IO ()
punchOutCommand [] = do
  eio <- entryIO
  elog <- entries eio
  punchOutIO eio
  hClose (outH eio)
  putStrLn ("Punched out of " ++ project elog)
  where
    project elog' = fromMaybe "NO PROJECT" $ projectName <$> lastEntry elog'
punchOutCommand _ = helpCommand ["out"]

summaryCommand :: [String] -> IO ()
summaryCommand args = do
  eio <- entryIO
  elog <- entries eio
  hClose (outH eio)
  case args of
    ["--csv"] -> putStrLn (showSummaryCSV $ summarizeEntries elog)
    _ -> putStrLn (showSummary $ summarizeEntries elog)
