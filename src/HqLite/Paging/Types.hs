module HqLite.Paging.Types where

import Data.Binary
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HM
import GHC.IO.Handle
import HqLite.Constants

-- * Pager and Page
type PageId = Word64

data Pager = Pager
    { pPageSize :: !Word64
    , pFileHandle :: !Handle
    , pCache :: Cache
    , pNumPages :: Word32
    }deriving(Show)

-- Pages are 4096 raw bytes
-- idk how to enforce this
newtype Page = Page
    { pData :: BS.ByteString
    }
    deriving (Show)

createPage :: BS.ByteString -> Page
createPage rawData
    | BS.length rawData /= fromIntegral pageSize = error "Page size mismatch"
    | otherwise = Page rawData

-- * Cache
newtype Cache = Cache
    { cPages :: HM.HashMap PageId Page
    }deriving(Show)
