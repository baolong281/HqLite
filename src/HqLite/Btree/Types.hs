{-# LANGUAGE RecordWildCards #-}

module HqLite.Btree.Types where

import Control.Monad.Cont
import Data.Binary
import Data.Binary.Put
import qualified Data.Vector as V
import HqLite.Constants (Row (..), pageSize, rowSize)

data TreeNode = LeafNode LeafData | InternalNode InternalData deriving (Show)

type Key = Word32

-- hardcoded idk how elsec to do this
leafHeaderSize :: Word32
leafHeaderSize = 1 + 1 + 4 + 4

leafMaxCells :: Word32
leafMaxCells = (fromIntegral pageSize - leafHeaderSize) `div` (1 + fromIntegral rowSize)

data LeafData = LeafData
    { lNodeType :: Bool
    , lIsRoot :: Bool
    , lParentPointer :: Word32
    , lNumCells :: Word32
    , lCells :: V.Vector (Key, Row)
    }
    deriving (Show)

newtype InternalData = InternalData
    {test :: Word32}
    deriving (Show)

instance Binary LeafData where
    put LeafData{..} = do
        putWord8 (if lNodeType then 1 else 0) -- Explicit 1 byte
        putWord8 (if lIsRoot then 1 else 0) -- Explicit 1 byte
        putWord32be lParentPointer
        putWord32be lNumCells
        mapM_ (\(k, v) -> put k >> put v) lCells
        -- 4 bytes for key and row
        let remainingBytes = fromIntegral pageSize - leafHeaderSize - lNumCells * (fromIntegral rowSize + 4)
        replicateM_ (fromIntegral remainingBytes) $ putWord8 0

    get = do
        nodeType <- get
        isRoot <- get
        parentPointer <- get
        numCells <- get
        cells <- replicateM (fromIntegral numCells) $ do
            key <- get
            row <- get
            return (key, row)
        return (LeafData nodeType isRoot parentPointer numCells (V.fromList cells))

initializeLeaf :: LeafData
initializeLeaf = LeafData False True 0 0 V.empty
