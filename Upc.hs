module Upc where

import Data.Map as M (Map, empty, insert, lookup)
import Text.Printf (printf)
import UpcParser
import Dot

data Loc = NodeName String
         | LabelName String

data UpcConfig = UpcConfig { ucRelabel :: Maybe (Int -> String -> String) }

config0 :: UpcConfig
config0 = UpcConfig { ucRelabel = Nothing }

patentConfig :: UpcConfig
patentConfig = UpcConfig { ucRelabel = Just patentLabel }

patentLabel :: Int -> String -> String
patentLabel ix l = printf "S%04d: %s" ((ix + 1) * 100) l

compile :: UpcConfig -> UpcProg -> Digraph
compile cfg (UpcProg stmts) =
  let (ns, lmap) = locate stmts
      ns'        = maybe ns (flip relabel ns) (ucRelabel cfg)
      es         = connect lmap stmts
      nstart     = Node "start" (nodeAttr0 { naLabel = Just "Start" })
      nend       = Node "end"   (nodeAttr0 { naLabel = Just "End" })
      estart     = Edge "start" (nodeNameAt 0) edgeAttr0
  in Digraph "flowchart" [] (nstart : ns' ++ [nend]) (estart : es) digraphAttr0

relabel :: (Int -> String -> String) -> [Node] -> [Node]
relabel f ns =
  [Node n (a { naLabel = Just (f ix l) })
    | (Node n a@(NodeAttr { naLabel = Just l }), ix) <- zip ns [0 ..]]

locate :: [Stmt] -> ([Node], Map String Loc)
locate = flip go ([], M.empty) . zip [0 ..]
  where
    go [] (ns, m) = (reverse ns, m)
    go ((ix, s@(SBranch p _ _)) : ss) (ns, m) =
      let a = nodeAttr0 { naLabel = Just p, naShape = stmtShape s }
          n = Node (nodeNameAt ix) a
      in go ss (n : ns, m)
    go ((_, SGoto _)  : ss) (ns, m) = go ss (ns, m)
    go ((_, SLabel l) : ss) (ns, m) = go ss (ns, M.insert l (topLoc ss) m)

    topLoc []                         = NodeName "end"
    topLoc ((ix, SBranch _ _ _) : _ ) = NodeName $ nodeNameAt ix
    topLoc ((_,  SGoto l)       : _ ) = LabelName l
    topLoc ((_,  SLabel _)      : ss) = topLoc ss

connect :: M.Map String Loc -> [Stmt] -> [Edge]
connect lmap = go . zip [0 ..]
  where
    go []                           = []
    go ((_,  SLabel _       ) : ss) = go ss
    go ((_,  SGoto _        ) : ss) = go ss
    go ((ix, SBranch _ yl nl) : ss) =
      let here   = nodeNameAt ix
          mey    = fmap (\l -> Edge here (nodeNameFor l) edgeAttr0 { eaLabel = Just "Yes" }) yl
          men    = fmap (\l -> Edge here (nodeNameFor l) edgeAttr0 { eaLabel = Just "No"  }) nl
          next l = Edge here (nextNodeName ss) edgeAttr0 { eaLabel = l }
      in case (mey, men) of
          (Nothing, Nothing) -> next Nothing           : go ss
          (Just ey, Nothing) -> ey : next (Just "No")  : go ss
          (Nothing, Just en) -> next (Just "Yes") : en : go ss
          (Just ey, Just en) -> ey : en                : go ss

    nodeNameFor l =
      case M.lookup l lmap of
        Nothing             -> error $ "connect: label \"" ++ l ++ "\" not found"
        Just (NodeName n)   -> n
        Just (LabelName l') -> nodeNameFor l'

    nextNodeName []                         = "end"
    nextNodeName ((_,  SLabel _     ) : ss) = nextNodeName ss
    nextNodeName ((_,  SGoto l      ) : _ ) = nodeNameFor l
    nextNodeName ((ix, SBranch _ _ _) : _ ) = nodeNameAt ix

nodeNameAt :: Int -> String
nodeNameAt = ("n" ++) . show

stmtShape :: Stmt -> Maybe Shape
stmtShape (SBranch _ Nothing Nothing) = Just Box
stmtShape (SBranch _ _       _      ) = Just Diamond
stmtShape (SGoto _)                   = Nothing
stmtShape (SLabel _)                  = Nothing

