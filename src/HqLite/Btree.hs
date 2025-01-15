{-# LANGUAGE RecordWildCards #-}

module HqLite.Btree where

import Control.Monad (replicateM)
import Data.Binary
import Data.ByteString.Lazy as BS
import HqLite.Paging.Page
import HqLite.Paging.Types
import HqLite.Table

data TreeNode = LeafNode LeafData | InternalNode InternalData

type Key = Word32

data LeafData = LeafData
    { lNodeType :: Bool
    , lIsRoot :: Bool
    , lParentPointer :: Word32
    , lNumCells :: Word32
    , lCells :: [(Key, Row)]
    }
    deriving (Show)

instance Binary LeafData where
    put LeafData{..} = do
        put lNodeType
        put lIsRoot
        put lParentPointer
        put lNumCells
        mapM_ (\(k, v) -> put k >> put v) lCells

    get = do
        nodeType <- get
        isRoot <- get
        parentPointer <- get
        numCells <- get
        cells <- replicateM (fromIntegral numCells) $ do
            key <- get
            row <- get
            return (key, row)
        return (LeafData nodeType isRoot parentPointer numCells cells)

data InternalData = InternalData
    { test :: Word32
    }

readNode :: Pager -> PageId -> IO (Maybe TreeNode)
readNode pager pageId = do
    page <- readPage pager pageId
    let rawPage = pData page
    case BS.index rawPage 0 of
        0x0 -> pure Nothing
        _ -> pure $ Just (LeafNode (decode rawPage))
