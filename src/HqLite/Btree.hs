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
import HqLite.Utils (binarySearch)

type CellData = V.Vector (Key, Row)

-- make better later
-- this can throw errors
-- will handle later
readNode :: Page -> TreeNode
readNode (Page raw)
    | index raw 0 == 0x1 = InternalNode (decode raw)
    | otherwise = LeafNode (decode raw)

-- insert and split into a leaf node
-- cells are evenly split between two new nodes
-- will call splitRoot and splitInternal to split its parent
leafSplitInsert :: Key -> Row -> CursorM ()
leafSplitInsert key row = do
    cursor <- get
    leafData <- liftIO $ getLeafNode cursor
    let oldKey = getLeafMax leafData

    let cells = insertSortedVec (lCells leafData) (\x y -> fst x < fst y) ((,) key row)

    let mid = V.length cells `div` 2
    let (first, second) = V.splitAt mid cells

    ( if lIsRoot leafData
            then splitRoot leafData first second
            else splitInternal oldKey (lParentPointer leafData) first second
        )

-- if leaf being split is a root, a new root is created and set to the split leaves parents
splitRoot :: LeafData -> CellData -> CellData -> CursorM ()
splitRoot oldLeaf leftData rightData = do
    Cursor{..} <- get

    let rightPage = getFreePage $ tPager cTable
    let rightNode = oldLeaf{lCells = rightData, lNumCells = fromIntegral (V.length rightData), lIsRoot = False, lParentPointer = 0, lNextLeaf = 0} -- change these to instead make new leaves, using old data like this is probably bad
    writeRight <- liftIO $ execStateT (writePage rightPage (createPage $ encode rightNode)) $ tPager cTable

    let leftPage = getFreePage writeRight
    let leftNode = oldLeaf{lCells = leftData, lNumCells = fromIntegral (V.length leftData), lIsRoot = False, lParentPointer = 0, lNextLeaf = rightPage}
    writeLeft <- liftIO $ execStateT (writePage leftPage (createPage $ encode leftNode)) writeRight

    let keys = V.fromList [(leftPage, getLeafMax leftNode)]
    let newRoot = InternalData True True 0 1 rightPage keys

    -- write the root to the cursor position. this should be the root
    writeRoot <- liftIO $ execStateT (writePage cPageNum (createPage $ encode newRoot)) writeLeft
    modify $ \c -> c{cTable = cTable{tPager = writeRoot}}

-- handle splits for children of internal nodes
splitInternal :: Key -> PageId -> CellData -> CellData -> CursorM ()
splitInternal oldKey parentPointer leftData rightData = do
    cursor@Cursor{..} <- get
    let rightKey = fst $ V.last rightData
    let leftKey = fst $ V.last leftData
    parent <- liftIO $ readNode <$> readPage (tPager cTable) parentPointer

    case parent of
        LeafNode _ -> error "Expected internal node"
        InternalNode parentNode -> do
            let rightPointer = getFreePage $ tPager cTable

            -- if this is the far right key, then the key from the old node shuold not be in the keypair list
            -- i hope 🙏
            let isRightKey = not (V.elem oldKey (V.map snd (iPointerKeys parentNode)))

            when (iNumKeys parentNode >= internalMaxKeys) (error "MAX KEYS CANNOT SPLIT HAVE NOT ADDED YET")

            -- we need to update the keypair list by first removing the old key of the old leaf
            let filteredKeys = V.filter (\pair -> snd pair /= oldKey) (iPointerKeys parentNode)
            let leftKeyPos = binarySearch (V.map snd filteredKeys) leftKey

            -- then we insert the new left key
            let keysWithNewLeft = insertAt leftKeyPos (cPageNum, leftKey) filteredKeys
            let rightKeyPos = binarySearch (V.map snd keysWithNewLeft) rightKey

            -- if the right node is the right most key we need to change that in the parent
            let newParent =
                    if isRightKey
                        then parentNode{iPointerKeys = keysWithNewLeft, iRightPointer = rightPointer, iNumKeys=fromIntegral (V.length keysWithNewLeft)}
                        else parentNode{iPointerKeys = insertAt rightKeyPos (rightPointer, rightKey) keysWithNewLeft, iNumKeys=fromIntegral (V.length keysWithNewLeft) + 1}

            currLeft <- liftIO $ getLeafNode cursor

            -- if its rightmost key next leaf is null, otherwise its the right key of the left node
            let rightNode =
                    if isRightKey
                    then createLeaf False parentPointer 0 (fromIntegral $ V.length rightData) rightData
                    else createLeaf False parentPointer (lNextLeaf currLeft) (fromIntegral $ V.length rightData) rightData

            let leftNode = createLeaf False parentPointer rightPointer (fromIntegral $ V.length leftData) leftData

            writeParent <- liftIO $ execStateT (writePage parentPointer (createPage $ encode newParent)) (tPager cTable)
            writeRight <- liftIO $ execStateT (writePage rightPointer (createPage $ encode rightNode)) writeParent

            -- left is written at current cursor page
            writeLeft <- liftIO $ execStateT (writePage cPageNum (createPage $ encode leftNode)) writeRight

            modify $ \c -> c{cTable = cTable{tPager = writeLeft}}

-- * General utils
insertAt :: Int -> a -> V.Vector a -> V.Vector a
insertAt i x xs = V.take i xs V.++ V.singleton x V.++ V.drop i xs

getLeafNode :: Cursor -> IO LeafData
getLeafNode Cursor{..} = do
    maybeNode <- readNode <$> readPage (tPager cTable) (fromIntegral cPageNum)
    case maybeNode of
        LeafNode leaf -> pure leaf
        _ -> error "Expected leaf node"

getInternalNode :: Cursor -> IO InternalData
getInternalNode Cursor{..} = do
    maybeNode <- readNode <$> readPage (tPager cTable) (fromIntegral cPageNum)
    case maybeNode of
        InternalNode internal -> pure internal
        _ -> error "Expected internal node"

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
