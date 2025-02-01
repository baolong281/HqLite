{-# LANGUAGE RecordWildCards #-}

module HqLite.Btree.Types where

import Control.Monad.Cont
import Control.Monad.State (StateT)
import Data.Binary
import Data.Binary.Put
import qualified Data.Vector as V
import HqLite.Constants (Row (..), pageSize, rowSize)
import HqLite.Paging.Types
import HqLite.Table.Types

data TreeNode = LeafNode LeafData | InternalNode InternalData deriving (Show)

type Key = Word32

-- hardcoded idk how elsec to do this
leafHeaderSize :: Word32
leafHeaderSize = 1 + 1 + 4 + 4 + 4

leafMaxCells :: Word32
leafMaxCells = (fromIntegral pageSize - leafHeaderSize) `div` (4 + fromIntegral rowSize)

data LeafData = LeafData
    { lNodeType :: Bool -- false means it is an internal node
    , lIsRoot :: Bool
    , lParentPointer :: PageId
    , lNextLeaf :: PageId -- 0 is considered null
    , lNumCells :: Word32
    , lCells :: V.Vector (Key, Row)
    }
    deriving (Show)

internalHeaderSize :: Word32
internalHeaderSize = 1 + 1 + 4 + 4 + 4

internalMaxKeys :: Word32
internalMaxKeys = (fromIntegral pageSize - internalHeaderSize) `div` 8 -- pointer + key is 8 bytes

data InternalData = InternalData
    { iNodeType :: Bool
    , iIsRoot :: Bool
    , iParentPointer :: PageId
    , iNumKeys :: Word32
    , iRightPointer :: PageId
    , iPointerKeys :: V.Vector (PageId, Key)
    }
    deriving (Show)

type CursorM a = StateT Cursor IO a

-- Rows will be 1-indexed why not
data Cursor = Cursor
    { cPageNum :: PageId
    , cCellNum :: Word32
    , cTable :: Table
    }
    deriving (Show)

instance Binary InternalData where
    put InternalData{..} = do
        putWord8 (if iNodeType then 1 else 0) -- Explicit 1 byte
        putWord8 (if iIsRoot then 1 else 0) -- Explicit 1 byte
        putWord32be iParentPointer
        putWord32be iNumKeys
        putWord32be iRightPointer
        mapM_ (\(k, v) -> put k >> put v) iPointerKeys
        -- 8 bytes for each key, pointer entry
        let remainingBytes = fromIntegral pageSize - internalHeaderSize - iNumKeys * 8
        replicateM_ (fromIntegral remainingBytes) $ putWord8 0

    get = do
        nodeType <- get
        isRoot <- get
        parentPointer <- get
        numKeys <- get
        rightPointer <- get
        pointerKeys <- replicateM (fromIntegral numKeys) $ do
            pointer <- get
            key <- get
            return (pointer, key)
        return (InternalData nodeType isRoot parentPointer numKeys rightPointer (V.fromList pointerKeys))

instance Binary LeafData where
    put LeafData{..} = do
        putWord8 (if lNodeType then 1 else 0) -- Explicit 1 byte
        putWord8 (if lIsRoot then 1 else 0) -- Explicit 1 byte
        putWord32be lParentPointer
        putWord32be lNextLeaf
        putWord32be lNumCells
        mapM_ (\(k, v) -> put k >> put v) lCells
        -- 4 bytes for key and row
        let remainingBytes = fromIntegral pageSize - leafHeaderSize - lNumCells * (fromIntegral rowSize + 4)
        replicateM_ (fromIntegral remainingBytes) $ putWord8 0

    get = do
        nodeType <- get
        isRoot <- get
        parentPointer <- get
        nextLeaf <- get
        numCells <- get
        cells <- replicateM (fromIntegral numCells) $ do
            key <- get
            row <- get
            return (key, row)
        return (LeafData nodeType isRoot parentPointer nextLeaf numCells (V.fromList cells))

-- DONT MAKE EVERYTHING ROOT!!!!!
-- CHANGE LATER!!!!!!!!!!!!!!!!!!!!!
initializeLeaf :: LeafData
initializeLeaf = LeafData False True 0 0 0 V.empty

createLeaf :: Bool -> PageId -> PageId -> Word32 -> V.Vector (Key, Row) -> LeafData
createLeaf = LeafData False