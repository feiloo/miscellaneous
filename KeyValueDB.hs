-- Vibe-coded with qwen-235B-2507 and allhands/openhands

-- Simple Persistent Key-Value Database in Haskell
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO
import System.Directory (doesFileExist, renameFile)
import Control.Exception (catch, SomeException)
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad (forever)

-- Type definition for our database
type Database = Map.Map String String
type Filename = String

-- Default filename for persistence
defaultFile :: Filename
defaultFile = "kvdb_persistent.dat"

-- Initialize an empty database
initDB :: Database
initDB = Map.empty

-- Insert a key-value pair into the database
insert :: String -> String -> Database -> Database
insert key value db = Map.insert key value db

-- Retrieve a value by key from the database
get :: String -> Database -> Maybe String
get key db = Map.lookup key db

-- Delete a key-value pair from the database
delete :: String -> Database -> Database
delete key db = Map.delete key db

-- Display all key-value pairs
display :: Database -> IO ()
display db = do
    putStrLn "Current database contents:"
    if Map.null db
        then putStrLn "  (empty)"
        else Map.foldlWithKey (\io k v -> io >> putStrLn ("  " ++ k ++ " -> " ++ v)) (return ()) db

-- Save database to file using atomic operations to prevent corruption
saveDB :: Filename -> Database -> IO ()
saveDB filename db = catch 
    (do
        let content = unlines $ Map.foldlWithKey (\acc k v -> acc ++ [k ++ ":" ++ v]) [] db
        let tempFilename = filename ++ ".tmp"
        -- Write to a temporary file first
        writeFile tempFilename content
        -- Atomically rename the temporary file to the final name
        -- This ensures that the file is either completely written or not at all
        renameFile tempFilename filename
        return ())
    (\e -> putStrLn $ "Warning: Could not save database: " ++ show (e :: SomeException))

-- Load database from file
loadDB :: Filename -> IO Database
loadDB filename = catch 
    (do
        fileExists <- doesFileExist filename
        if fileExists
            then do
                content <- readFile filename
                let pairs = map parseLine (filter (not . null) $ lines content)
                return $ Map.fromList pairs
            else do
                putStrLn $ "Database file " ++ filename ++ " not found. Starting with empty database."
                return initDB)
    (\e -> do
        putStrLn $ "Error loading database: " ++ show (e :: SomeException)
        putStrLn "Starting with empty database."
        return initDB)

-- Helper function to parse a line into a key-value pair
parseLine :: String -> (String, String)
parseLine line = 
    let (k, rest) = break (==':') line
        v = if null rest then "" else tail rest
    in (k, v)

-- Main function with a simple command-line interface
main :: IO ()
main = do
    putStrLn "Persistent Key-Value Database in Haskell with STM"
    putStrLn "Commands: put <key> <value>, get <key>, del <key>, list, quit"
    
    -- Create a TVar to hold the database
    dbVar <- newTVarIO initDB
    
    -- Load existing data if available
    initialDB <- loadDB defaultFile
    atomically $ writeTVar dbVar initialDB
    
    -- Start a background thread to handle save operations
    saveQueue <- newTChanIO
    let saveWorker = forever $ do
            db <- atomically $ readTChan saveQueue
            saveDB defaultFile db
            putStrLn $ "Database saved to " ++ defaultFile
    
    -- Run the save worker in the background
    _ <- async saveWorker
    
    -- Start the main command loop
    loop dbVar saveQueue
  where
    loop dbVar saveQueue = do
        putStr "> "
        hFlush stdout
        input <- getLine
        let command = words input
        case command of
            ("put":key:value:_) -> do
                -- Atomically update the database
                atomically $ do
                    db <- readTVar dbVar
                    let newDB = Map.insert key value db
                    writeTVar dbVar newDB
                    -- Send the updated database to the save queue
                    writeTChan saveQueue newDB
                putStrLn $ "Added " ++ key ++ " -> " ++ value
                loop dbVar saveQueue
            ["get", key] -> do
                -- Atomically read from the database
                value <- atomically $ do
                    db <- readTVar dbVar
                    return $ Map.lookup key db
                case value of
                    Just v -> putStrLn $ key ++ " -> " ++ v
                    Nothing -> putStrLn $ "Key '" ++ key ++ "' not found"
                loop dbVar saveQueue
            ["del", key] -> do
                -- Atomically update the database
                result <- atomically $ do
                    db <- readTVar dbVar
                    if Map.member key db
                        then do
                            let newDB = Map.delete key db
                            writeTVar dbVar newDB
                            writeTChan saveQueue newDB
                            return (True, newDB)
                        else return (False, db)
                case result of
                    (True, _) -> putStrLn $ "Deleted key '" ++ key ++ "'"
                    (False, _) -> putStrLn $ "Key '" ++ key ++ "' not found"
                loop dbVar saveQueue
            ["list"] -> do
                -- Atomically read the entire database
                db <- atomically $ readTVar dbVar
                putStrLn "Current database contents:"
                if Map.null db
                    then putStrLn "  (empty)"
                    else Map.foldlWithKey (\io k v -> io >> putStrLn ("  " ++ k ++ " -> " ++ v)) (return ()) db
                loop dbVar saveQueue
            ["quit"] -> do
                putStrLn "Goodbye!"
            [] -> loop dbVar saveQueue
            _ -> do
                putStrLn "Invalid command. Try: put <key> <value>, get <key>, del <key>, list, quit"
                loop dbVar saveQueue
