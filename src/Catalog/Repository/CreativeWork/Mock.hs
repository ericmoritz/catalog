-- | Mock respoitory for the CreativeWork entities

module Catalog.Repository.CreativeWork.Mock (newHandle)
where

import Catalog.Entity.CreativeWork (CreativeWork(..))
import qualified Catalog.Repository.CreativeWork as CreativeWorkRepo
import Control.Monad.Trans.Except
import Control.Applicative (empty)

-- TODO user an Identity monad to make this a proper mock repo

newHandle :: CreativeWorkRepo.Handle
newHandle = CreativeWorkRepo.Handle { CreativeWorkRepo.listAll = listAll, CreativeWorkRepo.findById = findById }


listAll :: CreativeWorkRepo.App [CreativeWork]
listAll = return
  [ CreativeWork "1" "Work 1" "Work 2 Desc" "author-1"
  , CreativeWork "2" "Work 2" "Work 2 Desc" "author-1"
  ]

findById :: String -> CreativeWorkRepo.App CreativeWork
findById id@"1" = return (CreativeWork id ("Work " ++ id) ("Work " ++ id ++ " Desc") "author-1")
findById id@"2" = return (CreativeWork id ("Work " ++ id) ("Work " ++ id ++ " Desc") "author-1")
findById _ = throwE CreativeWorkRepo.CreativeWorkNotFound
