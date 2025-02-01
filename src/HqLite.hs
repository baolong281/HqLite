module HqLite where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Vector as V
import HqLite.Btree
import HqLite.Btree.Types
import HqLite.Commands
import HqLite.Constants
import HqLite.Paging
import HqLite.Paging.Page (readPage)
import HqLite.Table
import HqLite.Table.Types (Table (tPager, tRootPage), TableM)
import System.IO
import Text.Printf (printf)
import Data.List (intercalate)

printPrompt :: IO ()
printPrompt = putStr "db> " >> hFlush stdout

parseCommand :: String -> Either String Command
parseCommand ('.' : cmd) = parseMetaCommand cmd
parseCommand cmd = parseSqlCommand cmd

formatList :: Show a => [a] -> String
formatList nums = "[" ++ intercalate ", " (map show nums) ++ "]"

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
                printf "%s- internal (size %d) %s\n" (indent depth) iNumKeys (formatList $ V.toList (V.map snd iPointerKeys))
                -- mapM_ (\child -> go child (depth + 1)) children

                mapM_
                    ( \cell -> do
                        nextNode <- readNode <$> readPage pager (fst cell)
                        go pager nextNode (depth + 2)
                    )
                    iPointerKeys

                right <- readNode <$> readPage pager iRightPointer
                go pager right (depth + 2)

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
            let _ = newCursorStart table
            selectedRows <- liftIO $ tableSelect table
            liftIO $ printTable (V.toList selectedRows)

printTable :: [Row] -> IO ()
printTable rows = do
    mapM_ print rows

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
