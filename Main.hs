{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Maybe (fromMaybe)
import System.Console.CmdArgs
import System.FilePath (takeBaseName)
import Dot (digraphToStr)
import UpcToDot (upcToDigraph, upcPatentConfig, upcConfig0)
import P_C (parseP_C)
import P_CToUpc (p_CToUpc)

data Option = Option { opPatent  :: Bool
		     , opOutFile :: Maybe String
                     , opInFile  :: String }
  deriving (Show, Data, Typeable)

option :: Option
option = Option { opPatent = False
		    &= help "Patent mode: add step # to nodes"
		    &= explicit 
		    &= name "patent"
		    &= name "p"
		, opOutFile = Nothing
		    &= help "Output file name"
		    &= explicit
		    &= name "out"
		    &= name "o"
		    &= typFile
		, opInFile = def
		    &= argPos 0
		    &= typ "INPUT_FILE" }
	  &= program "p-c"
	  &= summary "P--C-- Compiler, version 0.1.0"

main :: IO ()
main = do
  op <- cmdArgs option
  let infile  = opInFile op
  let outfile = fromMaybe (takeBaseName infile ++ ".dot") $ opOutFile op
  let upcCfg  = if opPatent op then upcPatentConfig else upcConfig0
  src <- readFile infile
  writeFile outfile $ digraphToStr
		    $ upcToDigraph upcCfg
		    $ p_CToUpc
		    $ fromRight
		    $ parseP_C src
  where
    fromRight (Right x) = x
    fromRight (Left x)  = error $ "fromRight: Left is given: " ++ show x

