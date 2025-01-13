{-# LANGUAGE RecordWildCards #-}
module HqLite.Cursor where
import Data.Binary (Word64, decode)

import HqLite.Table
import Control.Monad.State
import HqLite.Constants (pageSize)
import HqLite.Paging.Page
import HqLite.Paging

type CursorM a = State Cursor a

-- Rows will be 1-indexed why not
data Cursor = Cursor
    {
        cCurrentRow :: Word64
        , cTable :: Table
    }

newCursorEnd :: Table -> Cursor
newCursorEnd table = Cursor{cCurrentRow = getNumRows table, cTable = table}

newCursorStart :: Table -> Cursor
newCursorStart table = Cursor{cCurrentRow = 1, cTable = table}

next :: CursorM ()
next = do
    Cursor{..} <- get
    let newRow = cCurrentRow + 1
    put Cursor{cCurrentRow = newRow, ..}

prev :: CursorM ()
prev = do
    Cursor{..} <- get
    let newRow = cCurrentRow - 1
    put Cursor{cCurrentRow = newRow, ..}

getCurrentRow :: Cursor -> IO (Maybe Row)
getCurrentRow Cursor{..} = do
    putStrLn $ "nrows: " ++ show (getNumRows cTable)
    if getNumRows cTable < cCurrentRow then
        pure Nothing
    else do
        let 
            pager = tPager cTable
            rowLoc = rowSize * (fromIntegral cCurrentRow - 1)
            pageNum = rowLoc `div` pageSize
            offset = rowLoc `mod` pageSize

        rawPage <- readPage pager (fromIntegral pageNum)
        print $ pData rawPage
        print pageNum
        print offset
        pure $ Just (decode (readIndSize rawPage offset rowSize))