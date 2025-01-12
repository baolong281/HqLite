{-# LANGUAGE RecordWildCards #-}

module HqLite where

import HqLite.Table
import System.Exit (exitSuccess)
import System.IO
import Control.Monad.Reader
import HqLite.Commands
import Control.Monad.State


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
      liftIO $ print selectedRows  -- Print the selected rows
handleCommand (MetaCommand cmd) =
    liftIO $ evalMetaCommand cmd

selectFunc :: Table -> IO [Row]
selectFunc  = tableSelect

-- Execute SQL command (only modifies the table for Insert)
executeSQL :: SqlCommandType -> Table -> IO Table
executeSQL (Insert row) table = insertRow row table
executeSQL _ table = pure table  -- No-op for Select

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