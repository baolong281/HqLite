{-# LANGUAGE DeriveGeneric #-}

module HqLite.Table where

import Control.Monad.State
import Data.Text (Text)
import Data.ByteString.Lazy as BS
import HqLite.Paging (Page(..))
import Data.Maybe (isJust)
import HqLite.Constants
import HqLite.Paging.Page (emptyPage)
import Data.Binary
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

instance Binary Row

type Database = State Table

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


