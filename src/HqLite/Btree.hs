module HqLite.Btree where

import Control.Monad.State
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (index)
import qualified Data.Vector as V
import HqLite.Btree.Types
import HqLite.Constants (Row (..))
import HqLite.Paging.Page
import HqLite.Paging.Types
import HqLite.Table.Types

-- make better later
-- this can throw errors
-- will handle later
readNode :: Page -> TreeNode
readNode (Page raw)
    | index raw 0 == 0x1 = InternalNode (decode raw)
    | otherwise = LeafNode (decode raw)

-- insert and split into a leaf node
-- cells are evenly split between two new nodes
leafSplitInsert :: Key -> Row -> CursorM ()
leafSplitInsert key row = do
    cursor@Cursor{..} <- get
    leafData <- liftIO $ getLeafNode cursor

    let cells = insertSortedVec (lCells leafData) (\x y -> fst x < fst y) ((,) key row)

    let mid = V.length cells `div` 2
    let (first, second) = V.splitAt mid cells

    let newLeafRight = leafData{lCells = second, lNumCells = fromIntegral (V.length second), lIsRoot = False}

    -- write the right split into a new node
    let rightPageId = getFreePage $ tPager cTable
    writeRight <- liftIO $ execStateT (writePage rightPageId (createPage $ encode newLeafRight)) $ tPager cTable

    let newLeafLeft = leafData{lCells = first, lNumCells = fromIntegral (V.length first), lIsRoot = False, lNextLeaf = rightPageId}
    -- write the left data into the current node
    writeLeft <- liftIO $ execStateT (writePage cPageNum (createPage $ encode newLeafLeft)) writeRight

    modify $ \c -> c{cTable = cTable{tPager = writeLeft}}

    when (lIsRoot leafData) $ splitRoot rightPageId newLeafLeft

splitRoot :: PageId -> LeafData -> CursorM ()
splitRoot rightPointer leftLeaf = do
    Cursor{..} <- get

    let leftPage = getFreePage $ tPager cTable
    let keys = V.fromList [(leftPage, getLeafMax leftLeaf)]

    let newRoot = InternalData True True 0 1 rightPointer keys

    -- move the data currently in the root to a new page
    -- should be the left node of the split
    writeLeft <- liftIO $ execStateT (writePage leftPage (createPage $ encode leftLeaf)) (tPager cTable)

    -- write the root to the cursor position. this should be the root
    writeRoot <- liftIO $ execStateT (writePage cPageNum (createPage $ encode newRoot)) writeLeft

    modify $ \c -> c{cTable = cTable{tPager = writeRoot}}

-- * General utils
insertAt :: Int -> a -> V.Vector a -> V.Vector a
insertAt i x xs = V.take i xs V.++ V.singleton x V.++ V.drop i xs

getLeafNode :: Cursor -> IO LeafData
getLeafNode Cursor{..} = do
    maybeNode <- readNode <$> readPage (tPager cTable) (fromIntegral cPageNum)
    case maybeNode of
        LeafNode leaf -> pure leaf
        _ -> error "Expected leaf node"

-- get largest key in leafnode
getLeafMax :: LeafData -> Key
getLeafMax LeafData{..} = fst $ V.last lCells

-- insert an element into a sorted vector
insertSortedVec :: V.Vector a -> (a -> a -> Bool) -> a -> V.Vector a
insertSortedVec vec comp x =
    go V.empty vec
  where
    go acc xs
        | V.null xs = acc V.++ V.singleton x -- Base case: insert x at the end
        | comp x (V.head xs) = acc V.++ V.singleton x V.++ xs -- Insert x here
        | otherwise = go (acc V.++ V.singleton (V.head xs)) (V.tail xs)

-- * Cursor stuff
newCursorEnd :: Table -> IO Cursor
newCursorEnd table@Table{..} = do
    leafPage <- readPage tPager tRootPage
    let leaf = readNode leafPage
    case leaf of
        LeafNode LeafData{..} -> pure Cursor{cPageNum = tRootPage, cCellNum = lNumCells, cTable = table}
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
        LeafNode LeafData{..} -> do
            -- keep it 1-indexed
            -- if our index is too large return nothing
            if cCellNum - 1 >= lNumCells
                then pure Nothing
                else pure $ Just (snd (lCells V.! (fromIntegral cCellNum - 1)))
        _ -> error "Expected leaf node"

insertRow :: Row -> CursorM ()
insertRow row = do
    cursor <- get
    leaf <- liftIO $ getLeafNode cursor
    let key = fromIntegral $ rowId row
    if lNumCells leaf >= leafMaxCells
        then leafSplitInsert key row
        else updateLeafWithRow (fromIntegral $ rowId row) row leaf

updateLeafWithRow :: Key -> Row -> LeafData -> CursorM ()
updateLeafWithRow key row LeafData{..} = do
    Cursor{..} <- get
    -- cell nums indexed by one, we need to substract one i think
    let newCells = insertAt (fromIntegral cCellNum - 1) (key, row) lCells
        newLeaf = createPage $ encode LeafData{lCells = newCells, lNumCells = fromIntegral (length newCells), ..}
    updatedPager <- liftIO $ snd <$> runStateT (writePage cPageNum newLeaf) (tPager cTable)
    modify $ \c -> c{cTable = cTable{tPager = updatedPager}}
