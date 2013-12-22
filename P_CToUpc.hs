module P_CToUpc where

import P_C as P
import Upc as U
import Control.Monad.State
import Text.Printf (printf)

data CtxData = CtxData { cdNLabel :: Int }
type CtxState a = State CtxData a

p_CToUpc :: P_CProg -> UpcProg
p_CToUpc (P_CProg ss) =
  let s0 = CtxData { cdNLabel = 0 }
  in UpcProg $ concat $ evalState (mapM cStmt ss) s0 

cStmt :: P.Stmt -> CtxState [U.Stmt]
cStmt (SIfThen cond ys) = do
  lend <- genLabel
  ys'  <- concatMapM cStmt ys
  return $ [ SBranch cond Nothing (Just lend) ]
           ++ ys'
           ++ [ SLabel lend ]
cStmt (SIfThenElse cond ys ns) = do
  lelse <- genLabel
  lend  <- genLabel
  ys'   <- concatMapM cStmt ys
  ns'   <- concatMapM cStmt ns
  return $ [ SBranch cond Nothing (Just lelse) ]
           ++ ys'
           ++ [ SGoto lend
              , SLabel lelse ]
           ++ ns'
           ++ [ SLabel lend ]
cStmt (SWhile cond ss) = do
  lend <- genLabel
  ss'  <- concatMapM cStmt ss
  return $ [ SBranch cond Nothing (Just lend) ]
           ++ ss'
           ++ [ SLabel lend ]
cStmt (SRepeat ss cond) = do
  lbeg <- genLabel
  ss'  <- concatMapM cStmt ss
  return $ [ SLabel lbeg ]
           ++ ss'
           ++ [ SBranch cond Nothing (Just lbeg) ]
cStmt (SProc p) = do
  return $ [ SBranch p Nothing Nothing ]

genLabel :: CtxState String
genLabel = do
  s <- get
  let nl = cdNLabel s
  put $ s { cdNLabel = nl + 1 }
  return $ printf "L%06d" nl

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `liftM` mapM f xs

