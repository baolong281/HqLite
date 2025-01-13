{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HqLite.Table where

import Control.Exception (SomeException, catch)
import Control.Monad.State (StateT (runStateT))
import Data.Binary (Binary (..), decode, encode)
import Data.ByteString.Lazy as BS
    ( ByteString, drop, length, splitAt, take )
import Data.Int (Int64)
import Data.Text as T
import Data.Word (Word64)
import GHC.IO.IOMode (IOMode (ReadWriteMode))
import HqLite.Constants
import HqLite.Paging (Page (..), Pager)
import HqLite.Paging.Page (newPagerPlusSize, readPage, writePage)
import System.IO (openFile)

-- our tepmorary hardcoded table will be of form
-- id, username, email
-- int, varchar(32), varchar(255)
data Row = Row
    { rowId :: Int
    , rowUsername :: Text
    , rowEmail :: Text
    }
    deriving (Show, Eq)

rowSize :: Int64
rowSize = fromIntegral (BS.length (encode (Row 123 (T.pack "test") (T.pack "testing"))))

-- Pad or truncate a Text field to the fixed length
fixTextLength :: Int -> Text -> Text
fixTextLength len txt = T.take len (T.justifyLeft len ' ' txt)

-- Trim padding from a Text field
trimText :: Text -> Text
trimText = T.strip

instance Binary Row where
    put (Row user_id username email) = do
        put user_id
        put (fixTextLength 32 username)
        put (fixTextLength 255 email)
    get = do
        user_id <- get
        username <- get
        Row user_id (trimText username) . trimText <$> get

data Table = Table
    { tPager :: Pager
    , tBytesWritten :: Word64
    }

createTable :: FilePath -> IO Table
createTable path = do
    handle <- openFile path ReadWriteMode
    (pager, fileSize) <- newPagerPlusSize handle
    pure Table{tPager = pager, tBytesWritten = fileSize}

insertRow :: Row -> Table -> IO Table
insertRow row table@Table{..} = do
    let newData = encode row
        pageNum = tBytesWritten `div` fromIntegral pageSize
        offset = tBytesWritten `mod` fromIntegral pageSize

    page <- readPage tPager (fromIntegral pageNum)

    let updatedPage = alterPageOffset newData offset page

    case updatedPage of
        Nothing -> pure table
        Just updatedPage' -> do
            (_, newPager) <- runStateT (writePage pageNum updatedPage') tPager
            pure table{tPager = newPager, tBytesWritten = tBytesWritten + fromIntegral rowSize}

alterPageOffset :: BS.ByteString -> Word64 -> Page -> Maybe Page
alterPageOffset newData offset (Page currentData) =
    let
        end = BS.length newData + fromIntegral offset
     in
        if end <= pageSize
            then
                let (before, _) = BS.splitAt (fromIntegral offset) currentData
                    (_, after) = BS.splitAt end currentData
                    updatedData = before <> newData <> after
                 in Just $ Page updatedData
            else Nothing

getNumRows :: Table -> Word64
getNumRows Table{..} = tBytesWritten `div` fromIntegral rowSize

tableSelect :: Table -> IO [Row]
tableSelect Table{..} = do
    let totalPages = tBytesWritten `div` fromIntegral pageSize
        n = pageSize `div` fromIntegral rowSize -- Rows per full page
        -- Calculate the number of rows in the last page
        lastPageRows = (fromIntegral tBytesWritten `mod` fromIntegral pageSize) `div` rowSize

    putStrLn $ "total pages: " ++ show totalPages
    putStrLn $ "file size: " ++ show tBytesWritten

    -- Read all pages
    rows <-
        mapM
            ( \pageId -> do
                page <- readPage tPager (fromIntegral pageId)
                -- Determine the number of rows to read for this page
                let rowsToRead = if pageId == totalPages then lastPageRows else n
                let pageRows = getPageData page rowsToRead
                mapM (\binary -> catch (pure $ decode binary) (\(e :: SomeException) -> print e >> pure (Row 0 (T.pack "error") (T.pack $ show e)))) pageRows
            )
            [0 .. totalPages]

    pure $ Prelude.concat rows

getPageData :: Page -> Int64 -> [BS.ByteString]
getPageData page n =
    Prelude.map (\start -> readIndSize page (start * rowSize) rowSize) [0 .. n - 1]

readIndSize :: Page -> Int64 -> Int64 -> BS.ByteString
readIndSize (Page bs) offseet nBytes = BS.take (fromIntegral nBytes) (BS.drop (fromIntegral offseet) bs)
