module Upc where

import Data.Map as M (Map, empty, insert, lookup)
import Text.Printf (printf)
import UpcParser
import Dot

data Location = NodeName String
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
  let (ns, lab2nix) = locate stmts
      ns'           = maybe ns (flip addStepID ns) (ucRelabel cfg)
      es            = connect lab2nix stmts
      nstart        = Node "start" (nodeAttr0 { naLabel = Just "Start" })
      nend          = Node "end"   (nodeAttr0 { naLabel = Just "End" })
      estart        = Edge "start" (ixToNodeName 0) edgeAttr0
  in Digraph "flowchart" [] (nstart : ns' ++ [nend]) (estart : es) digraphAttr0

addStepID :: (Int -> String -> String) -> [Node] -> [Node]
addStepID f ns =
  [Node n (a {naLabel = Just (f ix l)})
    | (Node n a@(NodeAttr {naLabel = Just l}), ix) <- zip ns [0 ..]]

locate :: [Stmt] -> ([Node], Map String Location)
locate = flip go ([], M.empty) . zip [0 ..]
  where
    go :: [(Int, Stmt)] -> ([Node], Map String Location) -> ([Node], Map String Location)
    go [] (ns, m) = (reverse ns, m)
    go ((ix, s@(SBranch p _ _)) : ss) (ns, m) =
      let a = nodeAttr0 { naLabel = Just p, naShape = stmtShape s }
          n = Node (ixToNodeName ix) a
      in go ss (n : ns, m)
    go ((_, SGoto _)  : ss) (ns, m) = go ss (ns, m)
    go ((_, SLabel l) : ss) (ns, m) = go ss (ns, M.insert l (topLabel ss) m)

    topLabel :: [(Int, Stmt)] -> Location
    topLabel []                         = NodeName "end"
    topLabel ((ix, SBranch _ _ _) : _ ) = NodeName $ ixToNodeName ix
    topLabel ((_,  SGoto l)       : _ ) = LabelName l
    topLabel ((_,  SLabel _)      : ss) = topLabel ss

--locate :: Stmt -> (Int, [Node], Map String String) -> (Int, [Node], Map String String)
--locate s@(SBranch p _ _) (ix, ns, m) =
--  let a = nodeAttr0 { naLabel = Just p, naShape = stmtShape s }
--      n = Node (ixToNodeName ix) a
--  in (ix + 1, n : ns, m)
--locate (SLabel l) (ix, ns, m) =
--  let m' = M.insert l topLabel m
--  in (ix, ns, m')
--  where
--    topLabel | (Node s _ : _) <- ns = s
--             | []             <- ns = "end"
--             | otherwise            = error $ "topLabel: " ++ show ns

connect :: M.Map String Location -> [Stmt] -> [Edge]
connect labs = go . zip [0 ..]
  where
    go []                           = []
    go ((_,  SLabel _       ) : ss) = go ss
    go ((_,  SGoto _        ) : ss) = go ss
    go ((ix, SBranch _ yl nl) : ss) =
      let here   = ixToNodeName ix
          mey    = fmap (\l -> Edge here (nodeName l) edgeAttr0{ eaLabel = Just "Yes" }) yl
          men    = fmap (\l -> Edge here (nodeName l) edgeAttr0{ eaLabel = Just "No"  }) nl
          next l = Edge here (nextNodeName ss) edgeAttr0{ eaLabel = l }
      in case (mey, men) of
          (Nothing, Nothing) -> next Nothing           : go ss
          (Just ey, Nothing) -> ey : next (Just "No")  : go ss
          (Nothing, Just en) -> next (Just "Yes") : en : go ss
          (Just ey, Just en) -> ey : en                : go ss

    nodeName l =
      case M.lookup l labs of
        Nothing             -> error $ "connect: label \"" ++ l ++ "\" not found"
        Just (NodeName n)   -> n
        Just (LabelName l') -> nodeName l'

    nextNodeName []                         = "end"
    nextNodeName ((_,  SLabel _     ) : ss) = nextNodeName ss
    nextNodeName ((_,  SGoto l      ) : _ ) = nodeName l
    nextNodeName ((ix, SBranch _ _ _) : _ ) = ixToNodeName ix

--connect :: M.Map String String -> Stmt -> (Int, [Edge]) -> (Int, [Edge])
--connect _    (SLabel _)        acc      = acc
--connect labs (SBranch _ yl nl) (ix, es) =
--  let here   = ixToNodeName ix
--      mey    = fmap (\l -> Edge here (nodeName l) edgeAttr0{ eaLabel = Just "Yes" }) yl
--      men    = fmap (\l -> Edge here (nodeName l) edgeAttr0{ eaLabel = Just "No"  }) nl
--      next l = Edge here nextNodeName edgeAttr0{ eaLabel = l }
--  in case (mey, men) of
--      (Nothing, Nothing) -> (ix + 1, next Nothing                         : es)
--      (Just ey, Nothing) -> (ix + 1, ey                : next (Just "No") : es)
--      (Nothing, Just en) -> (ix + 1, next (Just "Yes") : en               : es)
--      (Just ey, Just en) -> (ix + 1, ey                : en               : es)
--  where
--    nodeName l = fromMaybe (error $ "connect: label \"" ++ l ++ "\" not found") $ M.lookup l labs
--    nextNodeName | Edge src _ _ : _ <- es = src
--                 | otherwise              = "end"

ixToNodeName :: Int -> String
ixToNodeName = ("n" ++) . show

stmtShape :: Stmt -> Maybe Shape
stmtShape (SBranch _ Nothing Nothing) = Just Box
stmtShape (SBranch _ _       _      ) = Just Diamond
stmtShape (SGoto _)                   = Nothing
stmtShape (SLabel _)                  = Nothing

