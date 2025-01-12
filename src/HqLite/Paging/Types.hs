module HqLite.Paging.Types where
import Data.Binary
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BS
import Data.Int
import GHC.IO.Handle

-- * Pager and Page
type PageId = Word64

data Pager = Pager
    { pPageSize :: !Word64
    , pFileHandle :: !Handle
    , pCache :: Cache
    }

-- Pages are 4096 raw bytes
-- idk how to enforce this
data Page = Page
    { pData :: BS.ByteString
    , pWritten :: Int64
    }

-- * Cache 
data Cache = Cache
    { 
        cPages :: HM.HashMap PageId Page
    }
