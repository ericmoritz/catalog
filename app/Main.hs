module Main where

import Catalog.Transport.HTTP (startApp)
import Catalog.Repository.CreativeWork.Mock (newHandle)

main :: IO ()
main = startApp newHandle
