module P_C where

import P_CParser as P
import UpcParser as U
import Control.Monad.State
import Text.Printf (printf)

data CtxData = CtxData { cdNLabel :: Int }
type CtxState a = State CtxData a

compile :: P_CProg -> UpcProg
compile (P_CProg ss) =
  let s0 = CtxData { cdNLabel = 0 }
  in U.UpcProg $ concat $ evalState (mapM cStmt ss) s0 

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
  lbeg <- genLabel
  lend <- genLabel
  ss'  <- concatMapM cStmt ss
  return $ [ SLabel lbeg
           , SBranch cond Nothing (Just lend) ]
           ++ ss'
           ++ [ SGoto lbeg
              , SLabel lend ]
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

