{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HqLite.Paging.Page where

import qualified Data.ByteString.Lazy as BS
import Data.Word (Word64)
import GHC.IO.Handle ( hSeek, SeekMode(AbsoluteSeek), Handle )
import qualified HqLite.Constants as Constants
import HqLite.Paging.Types
import HqLite.Paging.Cache
import Control.Monad.State

type PagerM a = StateT Pager IO a

emptyPage :: Page
emptyPage = Page{pData = BS.replicate Constants.pageSize 0x0, pWritten = 0}

emptyPager :: Handle -> Pager
emptyPager handle = Pager (fromIntegral Constants.pageSize) handle emptyCache

getOffset :: Pager -> PageId -> Word64
getOffset Pager{..} pageId = pageId * pPageSize

writePage :: PageId -> Page -> PagerM ()
writePage pageId (Page page _) = do
    pager@Pager{..} <- get
    let pageOffset = getOffset pager pageId
    liftIO $ do
            hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
            BS.hPut pFileHandle page
            pure ()
    -- Update cache
    put pager{pCache = insertPage pageId (Page page 0) pCache}

readPage :: PageId -> PagerM Page
readPage pageId = do
    pager@Pager{..} <- get
    case lookupPage pageId pCache of
        Just page -> pure page
        Nothing -> do
            let pageOffset = getOffset pager pageId
            liftIO $ hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
            rawData <- liftIO $ BS.hGet pFileHandle (fromIntegral pPageSize)
            pure $ Page rawData 0