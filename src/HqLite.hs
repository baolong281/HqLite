module HqLite where

import Control.Monad.Loops (unfoldrM)
import Control.Monad.Reader
import Control.Monad.State
import HqLite.Commands
import HqLite.Constants
import HqLite.Table
import System.IO
import HqLite.Table.Types (TableM, Table (tPager, tRootPage))
import HqLite.Btree.Types
import HqLite.Btree
import HqLite.Paging.Page (readPage)
import Text.Printf (printf)
import HqLite.Paging

printPrompt :: IO ()
printPrompt = putStr "db> " >> hFlush stdout

parseCommand :: String -> Either String Command
parseCommand ('.' : cmd) = parseMetaCommand cmd
parseCommand cmd = parseSqlCommand cmd

printTree :: Table -> IO ()
printTree table = do
    let pager = tPager table
    root <- readNode <$> readPage pager (tRootPage table)
    go pager root 0
    where
        go :: Pager -> TreeNode -> Int -> IO ()
        go pager node depth =
            case node of
                InternalNode InternalData{..} -> do
                    printf "%s- internal (size %d)\n" (indent depth) iNumKeys
                    -- mapM_ (\child -> go child (depth + 1)) children
                    right <- readNode <$> readPage pager iRightPointer
                    go pager right (depth + 2)

                    mapM_ (\cell -> do
                        nextNode <- readNode <$> readPage pager (fst cell)
                        go pager nextNode (depth + 2)
                        ) iPointerKeys

                LeafNode LeafData{..} -> do
                    printf "%s- leaf (size %d)\n" (indent depth) lNumCells
                    mapM_ (printf "%s- %d\n" (indent (depth + 2)) . fst) lCells

        indent :: Int -> String
        indent n = replicate n ' '

-- Command handler
handleCommand :: SqlCommandType -> TableM ()
handleCommand cmd =
    case cmd of
        Insert row -> do
            res <- tableInsert row
            case res of
                Left err -> liftIO $ putStrLn $ "Error: " ++ err
                Right _ -> do
                    liftIO $ putStrLn "Row inserted!"
        Select _ -> do
            table <- get
            let cursor = newCursorStart table
            selectedRows <- liftIO $ selectFunc cursor
            liftIO $ printTable selectedRows

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
            case cmd of
                MetaCommand Exit -> do
                     liftIO $ putStrLn "Bye!"
                MetaCommand Tree -> do
                    table <- get
                    liftIO $ printTree table
                    replLoop
                SqlCommand cmd' -> do
                    handleCommand cmd'
                    replLoop


-- Initialize and run
main :: IO ()
main = do
    -- handle <- openFile "./tmp/test.db" ReadWriteMode
    -- let pager = Pager 4096 handle
    table <- createTable "./tmp/test.db"
    _ <- execStateT replLoop table
    putStrLn "REPL exited."
