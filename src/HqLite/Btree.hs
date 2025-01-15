module HqLite.Btree where

import Data.Binary (decode)
import Data.ByteString.Lazy as BS
import HqLite.Btree.Types
import HqLite.Paging (Page (..))

-- make better later
readNode :: Page -> Maybe TreeNode
readNode page = Just (LeafNode (decode $ pData page))
