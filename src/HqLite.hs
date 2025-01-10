module HqLite where

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import HqLite.Paging
import System.Exit (exitSuccess)
import System.IO

-- our tepmorary hardcoded table will be of form
-- id, username, email
-- int, varchar(32), varchar(255)

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

main :: IO ()
main = do
    printPrompt
    command <- getLine
    case parseCommand command of
        Left err -> putStrLn err
        Right (MetaCommand cmd) -> evalMetaCommand cmd
        Right (SqlCommand cmd) -> do
            print cmd
    main
