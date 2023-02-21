module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  load_db <- DB.load
  case load_db of
    Success succ -> let
                  first = DB.findFirst (\en -> entryId en == getOptId getOpts) <$> load_db
                  in
                    case first of
                      Success succ2 -> case succ2 of
                                        Just anAnswer -> putStrLn $ entrySnippet anAnswer
                                        _ -> putStrLn "Failed to find"
                      _ -> putStrLn "Failed to find"
    Error err  -> putStrLn "Failed to load DB"  


-- | The fileName and id of the entry 
getNameAndID :: Entry -> String
getNameAndID entry = head $ lines $ show (FmtEntry entry)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  load_db <- DB.load
  case load_db of
    Success db ->
      let
        entries = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db
      in
        case entries of
          [] -> putStrLn "No entries found"
          _ -> putStrLn $ (unlines . L.map getNameAndID) entries
    Error err ->
      putStrLn "Failed to load DB"


-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  load_db <- DB.load
  case load_db of
    Success dab -> putStrLn "Entry with this content already exists: "
    Error err -> putStrLn "Failed to load DB"
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
