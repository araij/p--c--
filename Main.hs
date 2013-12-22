{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Maybe (fromMaybe)
import System.Console.CmdArgs
import System.FilePath (takeBaseName)
import Dot as D (compile)
import Upc as U (compile, patentConfig, config0)
import P_C as P (compile)
import P_CParser (parseP_C)

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
		    &= name "output"
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
  let upcCfg  = if opPatent op then U.patentConfig else U.config0
  src <- readFile infile
  writeFile outfile $ D.compile
		    $ U.compile upcCfg
		    $ P.compile
		    $ fromRight
		    $ parseP_C src
  where
    fromRight (Right x) = x
    fromRight (Left x)  = error $ "fromRight: Left is given: " ++ show x

