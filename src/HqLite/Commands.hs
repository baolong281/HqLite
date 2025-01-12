module HqLite.Commands where

import HqLite.Table
import qualified Data.Text as T
import Data.List(isInfixOf)

data Command
    = MetaCommand MetaCommandType
    | SqlCommand SqlCommandType
    deriving (Show)

data MetaCommandType = Exit deriving (Show)
data SqlCommandType
    = Insert Row
    | Select String
    deriving (Show)

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