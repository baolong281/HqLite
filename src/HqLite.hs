module HqLite where

import Control.Monad.Loops (unfoldrM)
import Control.Monad.Reader
import Control.Monad.State
import HqLite.Commands
import HqLite.Cursor
import HqLite.Table
import System.Exit (exitSuccess)
import System.IO

printPrompt :: IO ()
printPrompt = putStr "db> " >> hFlush stdout

parseCommand :: String -> Either String Command
parseCommand ('.' : cmd) = parseMetaCommand cmd
parseCommand cmd = parseSqlCommand cmd

evalMetaCommand :: MetaCommandType -> IO ()
evalMetaCommand Exit = putStrLn "Bye!" >> exitSuccess

type DbM a = StateT Table IO a

-- Command handler
handleCommand :: Command -> DbM ()
handleCommand (SqlCommand cmd) = do
    table <- get
    case cmd of
        Insert _ -> do
            table' <- liftIO $ executeSQL cmd table
            put table'
        Select _ -> do
            selectedRows <- liftIO $ selectFunc table
            liftIO $ printTable selectedRows
handleCommand (MetaCommand cmd) =
    liftIO $ evalMetaCommand cmd

printTable :: [Row] -> IO ()
printTable rows = do
    mapM_ print rows

selectFunc :: Table -> IO [Row]
selectFunc table = do
    let
        cursor = newCursorStart table
    unfoldrM fetchRow cursor
  where
    fetchRow :: Cursor -> IO (Maybe (Row, Cursor))
    fetchRow cursor = do
        row <- getCurrentRow cursor
        case row of
            Just row' -> do
                let nextCursor = execState next cursor
                return (Just (row', nextCursor))
            Nothing -> pure Nothing

-- Execute SQL command (only modifies the table for Insert)
executeSQL :: SqlCommandType -> Table -> IO Table
executeSQL (Insert row) table = insertRow row table
executeSQL _ table = pure table -- No-op for Select

-- Main REPL
replLoop :: DbM ()
replLoop = do
    liftIO printPrompt
    command <- liftIO getLine
    case parseCommand command of
        Left err -> do
            liftIO $ putStrLn err
            replLoop
        Right cmd -> do
            handleCommand cmd
            liftIO $ putStrLn "command executed!"
            liftIO $ print cmd
            replLoop

-- Initialize and run
main :: IO ()
main = do
    -- handle <- openFile "./tmp/test.db" ReadWriteMode
    -- let pager = Pager 4096 handle
    table <- createTable "./tmp/test.db"
    _ <- execStateT replLoop table
    putStrLn "REPL exited."
