{-# LANGUAGE RecordWildCards #-}

module HqLite.Cursor where

import Control.Monad.State
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BS
import Data.Word (Word32)
import HqLite.Btree
import HqLite.Btree.Types
import HqLite.Constants
import HqLite.Paging.Page
import HqLite.Paging.Types
import HqLite.Table

type CursorM a = StateT Cursor IO a

-- Rows will be 1-indexed why not
data Cursor = Cursor
    { cPageNum :: PageId
    , cCellNum :: Word32
    , cTable :: Table
    }

newCursorEnd :: Table -> IO Cursor
newCursorEnd table@Table{..} = do
    leafPage <- readPage tPager tRootPage
    let leaf = readNode leafPage
    case leaf of
        Nothing -> error "wtf"
        Just (LeafNode LeafData{..}) -> pure Cursor{cPageNum = tRootPage, cCellNum = lNumCells, cTable = table}
        _ -> error "wtf"

newCursorStart :: Table -> Cursor
newCursorStart table@Table{..} = Cursor{cPageNum = tRootPage, cCellNum = 1, cTable = table}

next :: CursorM ()
next = do
    Cursor{..} <- get
    let newCell = cCellNum + 1
    put Cursor{cCellNum = newCell, ..}

prev :: CursorM ()
prev = do
    Cursor{..} <- get
    let newCell = cCellNum - 1
    put Cursor{cCellNum = newCell, ..}

getCurrentRow :: Cursor -> IO (Maybe Row)
getCurrentRow Cursor{..} = do
    let pager = tPager cTable
    leafPage <- readPage pager cPageNum
    let leaf = readNode leafPage
    case leaf of
        Nothing -> error "do this later"
        Just (LeafNode LeafData{..}) -> do
            -- keep it 1-indexed
            if cCellNum - 1 >= lNumCells
                then pure Nothing
                else pure $ Just (snd (lCells !! (fromIntegral cCellNum - 1)))
        _ -> error "wtf"

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs =
    let (front, back) = splitAt i xs
     in front ++ (x : back)

insertRow :: Key -> Row -> CursorM ()
insertRow key row = do
    cursor@Cursor{..} <- get
    when (cCellNum >= leafMaxCells) $
        error "split not work"

    liftIO $ print leafMaxCells

    leaf <- liftIO $ getLeafNode cursor
    when (lNumCells leaf >= leafMaxCells) $ do
        error "split not work"

    liftIO $ print (lNumCells leaf)

    updateLeafWithRow key row leaf

getLeafNode :: Cursor -> IO LeafData
getLeafNode Cursor{..} = do
    maybeNode <- readNode <$> readPage (tPager cTable) (fromIntegral cPageNum)
    case maybeNode of
        Just (LeafNode leaf) -> pure leaf
        Nothing -> error "Page not found"
        Just _ -> error "Expected leaf node"

updateLeafWithRow :: Key -> Row -> LeafData -> CursorM ()
updateLeafWithRow key row LeafData{..} = do
    Cursor{..} <- get
    let newCells = insertAt (fromIntegral cCellNum) (key, row) lCells
        newLeaf = createPage $ encode LeafData{lCells = newCells, lNumCells = fromIntegral (length newCells), ..}
    updatedPager <- liftIO $ snd <$> runStateT (writePage cPageNum newLeaf) (tPager cTable)
    modify $ \c -> c{cTable = cTable{tPager = updatedPager}}
