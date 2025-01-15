{-# LANGUAGE ScopedTypeVariables #-}

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
import HqLite.Paging (Page (..), Pager)
import HqLite.Paging.Page
import HqLite.Paging.Types (PageId)
import System.IO (openFile)

data Table = Table
    { tPager :: Pager
    , tRootPage :: PageId
    }

type TableM a = StateT Table IO a

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
