module Main where

import System.Environment (getArgs)
import Dot (digraphToStr)
import UpcToDot
import P_C
import P_CToUpc

main :: IO ()
main = do
  [file] <- getArgs
  src    <- readFile file
  putStr $ digraphToStr
         $ upcToDigraph upcPatentConfig
         $ p_CToUpc
         $ fromRight $ parseP_C src
  where
    fromRight (Right x) = x
    fromRight (Left x)  = error $ "fromRight: Left is given: " ++ show x

