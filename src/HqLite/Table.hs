{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module HqLite.Table where

import Data.Text as T
import Data.ByteString.Lazy as BS
import HqLite.Paging (Page(..))
import Data.Maybe (isJust)
import HqLite.Constants
import HqLite.Paging.Page (emptyPage)
import Data.Binary (Binary(..), encode, decode)
import GHC.Generics (Generic)

-- our tepmorary hardcoded table will be of form
-- our tepmorary hardcoded table will be of form
-- id, username, email
-- int, varchar(32), varchar(255)
data Row = Row
    { rowId :: Int
    , rowUsername :: Text
    , rowEmail :: Text
    }
    deriving (Show, Generic)

rowSize :: Int
rowSize = fromIntegral (BS.length (encode ( Row 123 (T.pack "test") (T.pack "testing"))))

-- Pad or truncate a Text field to the fixed length
fixTextLength :: Int -> Text -> Text
fixTextLength len txt = T.take len (T.justifyLeft len ' ' txt)

-- Trim padding from a Text field
trimText :: Text -> Text
trimText = T.strip

-- Custom Binary instance for Row
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
    {
        tPages :: [Page]
    }

emptyTable :: Table
emptyTable = Table [emptyPage]

insertRow :: Row -> Table -> Maybe Table
insertRow row = insertTable (encode row)

insertTable :: BS.ByteString -> Table -> Maybe Table
insertTable newData table =
    tryWriteToPages newData (tPages table) >>= \updatedPages ->
    Just $ table { tPages = updatedPages }

tryWriteToPages :: BS.ByteString -> [Page] -> Maybe [Page]
tryWriteToPages newData pages =
    case Prelude.break (isJust . writeToFixedSize newData ) pages of
        (before, p:after) ->
            writeToFixedSize newData p >>= \updated ->
            Just (before ++ [updated] ++ after)
        _ -> Nothing

writeToFixedSize :: BS.ByteString -> Page -> Maybe Page
writeToFixedSize newData (Page currentData written)=
    let
        end = BS.length newData + written
    in if end <= pageSize
        then let (before, _) = BS.splitAt written currentData
                 (_, after) = BS.splitAt end currentData
                 updatedData = before <> newData <> after
            in Just $ Page updatedData end
        else Nothing

selectPage :: Page -> [Row]
selectPage page =
    let
        rawBytes = readPage page rowSize
    in
        Prelude.map decode rawBytes

readPage :: Page -> Int -> [BS.ByteString]
readPage page@Page{..} size =
    let n = fromIntegral pWritten `div` size
    in Prelude.map (\start -> readIndSize page (start * size) size) [0..n]

readIndSize :: Page -> Int -> Int -> BS.ByteString
readIndSize (Page bs _ ) i n = BS.take (fromIntegral n) (BS.drop (fromIntegral i) bs)


