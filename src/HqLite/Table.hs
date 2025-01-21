{-# LANGUAGE FlexibleContexts #-}

module HqLite.Table where

import Control.Monad.State.Lazy
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import HqLite.Constants
import HqLite.Paging.Page
import System.IO (openFile)
import HqLite.Btree.Types 
import HqLite.Btree
import HqLite.Table.Types
import Data.Vector as V
import HqLite.Paging
import HqLite.Paging.Types (PageId)

-- create table from filepath
createTable :: FilePath -> IO Table
createTable path = do
    handle <- openFile path ReadWriteMode
    pager <- newPager handle
    let pageId = 0
    pure Table{tPager = pager, tRootPage = pageId}

-- insert row into table
tableInsert :: Row -> TableM (Either String ())
tableInsert row = do
    table <- get
    cursor <- liftIO $ tableFind table (fromIntegral $ rowId row)
    handleCursor cursor
    where
        handleCursor cursor = do
            currentRow <- liftIO $ getCurrentRow cursor
            case currentRow of
                -- if there are no rows then we can just insert directly
                -- this is probably bad because if the cursor index is too large then this will still evaluate
                Nothing -> insertAndUpdateTable cursor
                Just row'
                    | rowId row' == rowId row -> pure $ Left "Cannot insert row. Row with existing key already found!"
                    | otherwise -> insertAndUpdateTable cursor

        insertAndUpdateTable cursor = do
            newTable <- cTable <$> liftIO (execStateT (insertRow row) cursor)
            Right <$> put newTable

tableLeaf :: Table -> IO (Either String LeafData)
tableLeaf Table{..} = do
    node <- readNode <$> readPage tPager tRootPage
    case node of
        LeafNode leaf -> pure $ Right leaf
        _ -> pure $ Left "Expected leaf node"

tableFind :: Table -> Key -> IO Cursor
tableFind table@Table{..} key = do
    node <- readNode <$> readPage tPager tRootPage
    go tPager node tRootPage
    where
        go :: Pager -> TreeNode -> PageId -> IO Cursor 
        go pager node pageId = do
            case node of 
                LeafNode leaf -> pure $ leafFind leaf key pageId table
                InternalNode internal -> do
                    let index = binarySearch (V.map snd (iPointerKeys internal)) key

                    let childPageId = 
                          if index == fromIntegral (iNumKeys internal)
                          then iRightPointer internal  -- if the index is the far right make it the right pointer
                          else fst (iPointerKeys internal V.! index)  -- otherwise use the pointer at index i

                    childNode <- readNode <$> readPage pager childPageId
                    go pager childNode childPageId

-- create cursor to row given a table key and page id
leafFind :: LeafData -> Key -> PageId -> Table -> Cursor
leafFind leaf key pageId =
    let
        keyVec = V.map fst $ lCells leaf
        -- the cursor is 1-indexed so we need to add one
        index = binarySearch keyVec key + 1
    in Cursor pageId (fromIntegral index)

-- * utils 
-- binary search for index containing item or index that should contain item
binarySearch :: Ord a => V.Vector a -> a -> Int
binarySearch vec key =
    let len = V.length vec
    in go 0 (len - 1)
    where
        go low high
            -- when search fails return the low index, which is where the key should be inserted
            | low > high = low
            | otherwise =
                let mid = (low + high) `div` 2
                    midVal = vec V.! mid
                in case compare key midVal of
                    LT -> go low (mid - 1)
                    GT -> go (mid + 1) high
                    EQ -> mid
