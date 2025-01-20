{-# LANGUAGE FlexibleContexts #-}

module HqLite.Table where

import Control.Monad.State.Lazy
import Data.ByteString.Lazy as BS (
    ByteString,
    drop,
    length,
    splitAt,
    take,
 )
import Data.Int (Int64)
import Data.Word (Word64)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import HqLite.Constants
import HqLite.Paging (Page (..))
import HqLite.Paging.Page
import System.IO (openFile)
import HqLite.Btree.Types 
import HqLite.Btree
import HqLite.Table.Types
import Data.Vector as V

createTable :: FilePath -> IO Table
createTable path = do
    handle <- openFile path ReadWriteMode
    pager <- newPager handle
    let pageId = 0
    pure Table{tPager = pager, tRootPage = pageId}

alterPageOffset :: BS.ByteString -> Word64 -> Page -> Maybe Page
alterPageOffset newData offset (Page currentData) =
    if end <= pageSize
        then
            let (before, _) = BS.splitAt (fromIntegral offset) currentData
                (_, after) = BS.splitAt end currentData
                updatedData = before <> newData <> after
             in Just $ Page updatedData
        else Nothing
  where
    end = BS.length newData + fromIntegral offset

getPageData :: Page -> Int64 -> [BS.ByteString]
getPageData page n =
    Prelude.map (\start -> readIndSize page (start * rowSize) rowSize) [0 .. n - 1]

readIndSize :: Page -> Int64 -> Int64 -> BS.ByteString
readIndSize (Page bs) offseet nBytes = BS.take (fromIntegral nBytes) (BS.drop (fromIntegral offseet) bs)

tableInsert :: Row -> TableM (Either String ())
tableInsert row = do
    table <- get
    maybeCursor <- liftIO $ tableFind table (fromIntegral $ rowId row)
    case  maybeCursor of
        Left err -> pure $ Left err
        Right cursor -> handleCursor cursor
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

tableFind :: Table -> Key -> IO (Either String Cursor)
tableFind table key = do
    result <- tableLeaf table
    case result of
        Left err -> pure $ Left err
        Right leaf -> pure $ Right $ leafFind table leaf key

leafFind :: Table -> LeafData -> Key -> Cursor
leafFind table leaf key =
    let
        keyVec = V.map fst $ lCells leaf
        -- the cursor is 1-indexed so we need to add one
        index = binarySearch keyVec key + 1
    in Cursor (tRootPage table) (fromIntegral index) table

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
