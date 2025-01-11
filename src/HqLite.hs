{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HqLite where

import Data.List (isInfixOf)
import qualified Data.Text as T
import HqLite.Paging (Pager (..))
import HqLite.Table
import System.Exit (exitSuccess)
import System.IO
import Data.IORef
import Control.Monad.Reader

data Command
    = MetaCommand MetaCommandType
    | SqlCommand SqlCommandType
    deriving (Show)

data MetaCommandType = Exit deriving (Show)
data SqlCommandType
    = Insert Row
    | Select String
    deriving (Show)

printPrompt :: IO ()
printPrompt = putStr "db> " >> hFlush stdout

parseCommand :: String -> Either String Command
parseCommand ('.' : cmd) = parseMetaCommand cmd
parseCommand cmd = parseSqlCommand cmd

parseMetaCommand :: String -> Either String Command
parseMetaCommand cmd =
    MetaCommand <$> case cmd of
        "exit" -> Right Exit
        _ -> Left $ "Invalid meta command: " ++ cmd

parseSqlCommand :: String -> Either String Command
parseSqlCommand cmd
    | "insert" `isInfixOf` cmd = case parseInsert cmd of
        Left err -> Left err
        Right insert -> Right $ SqlCommand $ Insert insert
    | "select" `isInfixOf` cmd = Right $ SqlCommand $ Select ""
    | otherwise = Left $ "Invalid SQL command: " ++ cmd

parseInsert :: String -> Either String Row
parseInsert cmd = do
    let parts = words cmd
    if length parts /= 4
        then Left "Invalid INSERT command: expected 4 parts (insert, id, username, email)"
        else do
            let idStr = parts !! 1
            let username = T.pack $ parts !! 2
            let email = T.pack $ parts !! 3
            case reads idStr of
                [(id_num, "")] -> Right $ Row id_num username email
                _ -> Left $ "Invalid ID: " ++ idStr

evalMetaCommand :: MetaCommandType -> IO ()
evalMetaCommand Exit = putStrLn "Bye!" >> exitSuccess

-- Environment containing all our stateful components
data DbEnv = DbEnv 
    { dbTable :: IORef Table
    , dbPager :: Pager
    }

-- Our monad stack
newtype DbM a = DbM { runDbM :: ReaderT DbEnv IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader DbEnv)

-- Helper functions to access state
getTable :: DbM Table
getTable = do
    ref <- asks dbTable
    liftIO $ readIORef ref

modifyTable :: (Table -> Table) -> DbM ()
modifyTable f = do
    ref <- asks dbTable
    liftIO $ modifyIORef' ref f

-- Command handlers
handleCommand :: Command -> DbM ()
handleCommand (SqlCommand cmd) = do
    table <- getTable
    let newTable = executeSQL cmd table
    modifyTable (const newTable)

handleCommand (MetaCommand cmd) = 
    liftIO $ evalMetaCommand cmd

executeSQL :: SqlCommandType -> Table -> Table
executeSQL cmd table =
    case cmd of
        Insert row -> case insertRow row table of
            Just newTable -> newTable
            Nothing -> table
        Select _ -> table 

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
    handle <- openFile "./tmp/test.db" ReadWriteMode
    let pager = Pager 4096 handle
    tableRef <- newIORef emptyTable
    let env = DbEnv tableRef pager
    runReaderT (runDbM replLoop) env
    putStrLn "REPL exited."