module HqLite.Paging.Page where

import Data.Binary
import Data.Text (Text)
import Numeric.Natural

data Table = Table
    { tNPages :: Natural
    , tPages :: [Put]
    }

data Row = Row
    { iId :: Int
    , iUsername :: Text
    , iEmail :: Text
    }
    deriving (Show)

instance Binary Row where
    put (Row i u e) = put i >> put u >> put e
    get = Row <$> get <*> get <*> get
