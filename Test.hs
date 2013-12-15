module Main where

import System.Environment (getArgs)
import Upc (parseUpc)
import Dot (digraphToStr)
import UpcToDot

main :: IO ()
main = do
  [file] <- getArgs
  src    <- readFile file
  putStr $ digraphToStr $ upcToDigraph upcPatentConfig $ fromRight $ parseUpc src
  where
    fromRight (Right x) = x
    fromRight (Left x)  = error $ "fromRight: Left is given: " ++ show x
