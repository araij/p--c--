module Main where

import System.Environment (getArgs)
import Dot as D (compile)
import Upc as U (compile, patentConfig)
import UpcParser (parseUpc)

main :: IO ()
main = do
  [file] <- getArgs
  src    <- readFile file
  putStr $ D.compile $ U.compile U.patentConfig $ fromRight $ parseUpc src
  where
    fromRight (Right x) = x
    fromRight (Left x)  = error $ "fromRight: Left is given: " ++ show x
