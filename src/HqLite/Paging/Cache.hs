module HqLite.Paging.Cache where

import qualified Data.HashMap.Strict as HM
import HqLite.Paging.Types
emptyCache :: Cache
emptyCache = Cache HM.empty

insertPage :: PageId -> Page -> Cache -> Cache
insertPage pid page (Cache pages) = 
    Cache $ HM.insert pid page pages

lookupPage :: PageId -> Cache -> Maybe Page
lookupPage pid (Cache pages) = 
    HM.lookup pid pages

evictPage :: PageId -> Cache -> Cache
evictPage pid (Cache pages) =
    Cache $ HM.delete pid pages
