-- | types for the CreativeWork repository

module Catalog.Repository.CreativeWork (runApp, App, Error(..), Handle(..))
where

import Control.Monad.Trans.Except
import Catalog.Entity.CreativeWork

type App a = ExceptT Error IO a

data Error = CreativeWorkNotFound | CreativeWorkInternalError String

runApp :: App a -> IO (Either Error a)
runApp = runExceptT

data Handle = Handle { listAll :: App [ CreativeWork ]
                     , findById :: String -> App CreativeWork
                     }
