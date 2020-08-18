-- | types for the CreativeWork repository

module Catalog.Repository.CreativeWork (runCreativeWorkRepoApp, CreativeWorkRepoApp, CreativeWorkRepoError(..))
where

import Control.Monad.Trans.Except

type CreativeWorkRepoApp a = ExceptT CreativeWorkRepoError IO a

data CreativeWorkRepoError = CreativeWorkNotFound | CreativeWorkInternalError String

runCreativeWorkRepoApp :: CreativeWorkRepoApp a -> IO (Either CreativeWorkRepoError a)
runCreativeWorkRepoApp = runExceptT
