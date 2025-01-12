module HqLite.Paging.Types where
import Data.Binary
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BS
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
newtype Page = Page
    { pData :: BS.ByteString
    }

-- * Cache 
data Cache = Cache
    { 
        cPages :: HM.HashMap PageId Page
    }
