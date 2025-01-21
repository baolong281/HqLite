{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HqLite.Paging.Page where

import Control.Monad.State
import Data.Binary (encode, Word32)
import qualified Data.ByteString.Lazy as BS
import Data.Word (Word64)
import GHC.IO.Handle (Handle, SeekMode (AbsoluteSeek), hSeek)
import HqLite.Btree.Types
import qualified HqLite.Constants as Constants
import HqLite.Paging.Cache
import HqLite.Paging.Types
import System.IO (hFileSize, hFlush)

type PagerM a = StateT Pager IO a

newPager :: Handle -> IO Pager
newPager handle = do
    numPages <- getNumPages handle
    let emptyPager = Pager (fromIntegral Constants.pageSize) handle emptyCache numPages
    if numPages == 0
        then initializeEmptyPager emptyPager
        else pure emptyPager
  where
    initializeEmptyPager pager = do
        let rootPage = Page $ encode initializeLeaf
        snd <$> runStateT (writePage 0 rootPage) pager

getNumPages :: Handle -> IO Word32
getNumPages handle = do
    let pageSize = fromIntegral Constants.pageSize
    (`div` pageSize) . fromIntegral <$> hFileSize handle

getOffset :: Pager -> PageId -> Word64
getOffset Pager{..} pageId = fromIntegral pageId * pPageSize

writePage :: PageId -> Page -> PagerM ()
writePage pageId (Page page) = do
    pager@Pager{..} <- get
    let pageOffset = getOffset pager pageId
    liftIO $ do
        hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
        BS.hPut pFileHandle page >> hFlush pFileHandle
        pure ()
    -- Update cache
    numPages <- liftIO $ getNumPages pFileHandle
    put pager{pCache = insertPage pageId (Page page) pCache, pNumPages = numPages}

-- this should probably update the cache
-- do laterc
readPage :: Pager -> PageId -> IO Page
readPage pager@Pager{..} pageId = do
    case lookupPage pageId pCache of
        Just page -> pure page
        Nothing -> do
            let pageOffset = getOffset pager pageId
            hSeek pFileHandle AbsoluteSeek (fromIntegral pageOffset)
            rawData <- liftIO $ BS.hGet pFileHandle (fromIntegral pPageSize)
            pure $ Page rawData

getFreePage :: Pager -> PageId
getFreePage Pager{..} = fromIntegral pNumPages