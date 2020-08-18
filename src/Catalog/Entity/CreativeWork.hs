-- | The types for the CreativeWorks

module Catalog.Entity.CreativeWork ( CreativeWork(..) )
where

data CreativeWork = CreativeWork
  { creativeWorkId :: String
  , creativeWorkName :: String
  , creativeWorkDesc :: String
  ,  creativeWorkAuthorId :: String
  } deriving (Eq, Show)
