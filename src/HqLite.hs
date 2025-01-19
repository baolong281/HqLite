module HqLite where

import Control.Monad.Loops (unfoldrM)
import Control.Monad.Reader
import Control.Monad.State
import HqLite.Commands
import HqLite.Constants
import HqLite.Cursor
import HqLite.Table
import System.Exit (exitSuccess)
import System.IO
import HqLite.Table.Types (TableM)

printPrompt :: IO ()
printPrompt = putStr "db> " >> hFlush stdout

parseCommand :: String -> Either String Command
parseCommand ('.' : cmd) = parseMetaCommand cmd
parseCommand cmd = parseSqlCommand cmd

evalMetaCommand :: MetaCommandType -> IO ()
evalMetaCommand Exit = putStrLn "Bye!" >> exitSuccess

-- Command handler
handleCommand :: Command -> TableM ()
handleCommand (SqlCommand cmd) =
    case cmd of
        Insert row -> do
            res <- tableInsert row
            case res of 
                Left err -> liftIO $ putStrLn $ "Error: " ++ err
                Right _ -> return ()
        Select _ -> do
            table <- get
            let cursor = newCursorStart table
            selectedRows <- liftIO $ selectFunc cursor
            liftIO $ printTable selectedRows
handleCommand (MetaCommand cmd) =
    liftIO $ evalMetaCommand cmd

printTable :: [Row] -> IO ()
printTable rows = do
    mapM_ print rows

selectFunc :: Cursor -> IO [Row]
selectFunc cursor = do
    let
        table = cTable cursor
        cursorStart = newCursorStart table
    unfoldrM fetchRow cursorStart
  where
    fetchRow :: Cursor -> IO (Maybe (Row, Cursor))
    fetchRow currCursor = do
        row <- getCurrentRow currCursor
        case row of
            Just row' -> do
                nextCursor <- execStateT next currCursor
                return (Just (row', nextCursor))
            Nothing -> pure Nothing

replLoop :: TableM ()
replLoop = do
    liftIO printPrompt
    command <- parseCommand <$> liftIO getLine
    case command of
        Left err -> do
            liftIO $ putStrLn err
            replLoop
        Right cmd -> do
            handleCommand cmd
            replLoop

-- Initialize and run
main :: IO ()
main = do
    -- handle <- openFile "./tmp/test.db" ReadWriteMode
    -- let pager = Pager 4096 handle
    table <- createTable "./tmp/test.db"
    _ <- execStateT replLoop table
    putStrLn "REPL exited."
