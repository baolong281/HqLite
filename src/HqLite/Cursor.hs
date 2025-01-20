module HqLite.Cursor where

import Control.Monad.State
import Data.Binary (encode)
import Data.Word (Word32)
import HqLite.Btree
import HqLite.Btree.Types
import HqLite.Constants
import HqLite.Paging.Page
import HqLite.Paging.Types
import HqLite.Table.Types
import qualified Data.Vector as V

type CursorM a = StateT Cursor IO a

-- Rows will be 1-indexed why not
data Cursor = Cursor
    { cPageNum :: PageId
    , cCellNum :: Word32
    , cTable :: Table
    } deriving(Show)

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
            -- if our index is too large return nothing
            if cCellNum - 1 >= lNumCells
                then pure Nothing
                else pure $ Just (snd (lCells V.! (fromIntegral cCellNum - 1)))
        _ -> error "wtf"

insertAt :: Int -> a -> V.Vector a -> V.Vector a
insertAt i x xs = V.take i xs V.++ V.singleton x V.++ V.drop i xs
    


insertRow :: Row -> CursorM ()
insertRow row = do
    cursor@Cursor{..} <- get
    when (cCellNum >= leafMaxCells) $
        error "split not work"

    leaf <- liftIO $ getLeafNode cursor
    when (lNumCells leaf >= leafMaxCells) $ do
        error "split not work"

    updateLeafWithRow (fromIntegral $ rowId row) row leaf

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
    -- cell nums indexed by one, we need to substract one i think
    let newCells = insertAt (fromIntegral cCellNum - 1) (key, row) lCells
        newLeaf = createPage $ encode LeafData{lCells = newCells, lNumCells = fromIntegral (length newCells), ..}
    updatedPager <- liftIO $ snd <$> runStateT (writePage cPageNum newLeaf) (tPager cTable)
    modify $ \c -> c{cTable = cTable{tPager = updatedPager}}
