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
import qualified Catalog.Repository.CreativeWork as CreativeWorkRepo


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

startApp :: CreativeWorkRepo.Handle -> IO ()
startApp worksRepoHandle = do
  putStrLn "Listening on :8000"
  run 8000 $ app worksRepoHandle

app :: CreativeWorkRepo.Handle -> Application
app worksRepoHandle = serve api $ server worksRepoHandle

api :: Proxy API
api = Proxy

server :: CreativeWorkRepo.Handle -> ServerT API Handler
server worksRepoHandle = worksServer worksRepoHandle

-- CreativeWorks API
worksServer worksRepoHandle = hoistServer api workAppToHandler (works :<|> findWork)
  where
    works = map workToCreativeWorkResponse <$> (CreativeWorkRepo.listAll worksRepoHandle)
    findWork id = workToCreativeWorkResponse <$> (CreativeWorkRepo.findById worksRepoHandle id)

-- This maps our custom CreativeWorkRepo.App into a Handler for servant
workAppToHandler :: CreativeWorkRepo.App a -> Handler a
workAppToHandler x = do
  eitherErrorOrRes <- liftIO $ fmap (mapBoth workRepoErrorToServerError id) $ CreativeWorkRepo.runApp x
  either throwError return eitherErrorOrRes

workToCreativeWorkResponse cw = CreativeWorkResponse { url = "/works/" ++ creativeWorkId cw
                                                     , name = creativeWorkName cw
                                                     , description = creativeWorkDesc cw
                                                     , author = "/authors/" ++ creativeWorkAuthorId cw
                                                     }

-- TODO: Make these a standard JSON error type
workRepoErrorToServerError CreativeWorkRepo.CreativeWorkNotFound = err404
workRepoErrorToServerError (CreativeWorkRepo.CreativeWorkInternalError _) = err500
