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
import System.IO (hFileSize)

type PagerM a = StateT Pager IO a

newPagerPlusSize :: Handle -> IO (Pager, Word64)
newPagerPlusSize handle = do
    let pageSize = fromIntegral Constants.pageSize
    fileSize <- hFileSize handle
    pure (Pager pageSize handle emptyCache, fromIntegral fileSize)

getOffset :: Pager -> PageId -> Word64
getOffset Pager{..} pageId = pageId * pPageSize

writePage :: PageId -> Page -> PagerM ()
writePage pageId (Page page) = do
    pager@Pager{..} <- get
    let pageOffset = getOffset pager pageId
    liftIO $ do
            hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
            BS.hPut pFileHandle page
            pure ()
    -- Update cache
    put pager{pCache = insertPage pageId (Page page) pCache}

readPage :: Pager -> PageId -> IO Page
readPage pager@Pager{..} pageId = do
    case lookupPage pageId pCache of
        Just page -> pure page
        Nothing -> do
            let pageOffset = getOffset pager pageId
            hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
            rawData <- liftIO $ BS.hGet pFileHandle (fromIntegral pPageSize)
            pure $ Page rawData