-- | Implements the RESTful HTTP transpory for the CreativeWorkResponse and Authors services
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Catalog.Transport.HTTP where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Applicative (empty)
import Control.Monad.Trans.Except
import Data.Either.Combinators (mapBoth)

import Catalog.Entity.CreativeWork (CreativeWork(..))
import Catalog.Repository.CreativeWork (runCreativeWorkRepoApp, CreativeWorkRepoApp, CreativeWorkRepoError(..))
-- TODO: figure out how to DI the repository
import qualified Catalog.Repository.CreativeWork.Mock as CreativeWorkRepo


data CreativeWorkResponse = CreativeWorkResponse
  { url :: String
  , name :: String
  , description :: String
  ,  author :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''CreativeWorkResponse)

type API =
  "works" :> (
    Get '[JSON] [CreativeWorkResponse]
    :<|> Capture "id" String :> Get '[JSON] CreativeWorkResponse
    )

startApp :: IO ()
startApp = do
  putStrLn "Listening on :8000"
  run 8000 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: ServerT API Handler
server = worksServer

-- CreativeWorks API
worksServer = hoistServer api workAppToHandler (works :<|> findWork)
  where
    works = map workToCreativeWorkResponse <$> CreativeWorkRepo.listAll
    findWork id = workToCreativeWorkResponse <$> (CreativeWorkRepo.findById id)

-- This maps our custom CreativeWorkRepoApp into a Handler for servant
workAppToHandler :: CreativeWorkRepoApp a -> Handler a
workAppToHandler x = do
  eitherErrorOrRes <- liftIO $ fmap (mapBoth workRepoErrorToServerError id) $ runCreativeWorkRepoApp x
  either throwError return eitherErrorOrRes

workToCreativeWorkResponse cw = CreativeWorkResponse { url = "/works/" ++ creativeWorkId cw
                                                     , name = creativeWorkName cw
                                                     , description = creativeWorkDesc cw
                                                     , author = "/authors/" ++ creativeWorkAuthorId cw
                                                     }

-- TODO: Make these a standard JSON error type
workRepoErrorToServerError CreativeWorkNotFound = err404
workRepoErrorToServerError (CreativeWorkInternalError _) = err500
