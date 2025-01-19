module HqLite.Constants (pageSize, rowSize, Row (..)) where

import Data.Binary
import Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Data.Text as T

pageSize :: Int64
pageSize = 4096

-- our tepmorary hardcoded table will be of form
-- id, username, email
-- int, varchar(32), varchar(255)
data Row = Row
    { rowId :: Int
    , rowUsername :: Text
    , rowEmail :: Text
    }
    deriving (Eq)

instance Show Row where
    show (Row rowId username email) = 
        "(" ++ show rowId ++ ", " ++ T.unpack username ++ ", " ++ T.unpack email ++ ")"

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
