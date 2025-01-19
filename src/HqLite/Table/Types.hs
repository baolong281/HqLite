module HqLite.Table.Types where
import Control.Monad.State (StateT)
import HqLite.Paging (Pager)
import HqLite.Paging.Types (PageId)

data Table = Table
    { tPager :: Pager
    , tRootPage :: PageId
    }deriving(Show)

type TableM a = StateT Table IO a