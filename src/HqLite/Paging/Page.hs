{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HqLite.Paging.Page where

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word64)
import GHC.IO.Handle
import Data.Int (Int64)
import qualified HqLite.Constants as Constants

-- Pages are 4096 raw bytes
data Page = Page 
    {
        pData :: BS.ByteString,
        pWritten :: Int64
    }

emptyPage :: Page
emptyPage = Page {pData = BS.replicate Constants.pageSize 0x0, pWritten = 0}

type PageId = Word64

data Pager = Pager
    { pPageSize :: !Word64
    , pFileHandle :: !Handle
    }

getOffset :: Pager -> PageId -> Word64
getOffset Pager{..} pageId = pageId * pPageSize

writePage :: Pager -> PageId -> Page -> IO ()
writePage pager@Pager{..} pageId (Page page _) =
    let pageOffset = getOffset pager pageId
     in do
            hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
            BS.hPut pFileHandle page
            pure ()

readPage :: Pager -> PageId -> IO Page
readPage pager@Pager{..} pageId = do
    let pageOffset = getOffset pager pageId
    hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
    rawData <- BS.hGet pFileHandle (fromIntegral pPageSize)
    pure $ Page rawData 0

