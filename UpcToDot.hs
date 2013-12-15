module UpcToDot where

import Data.Map as M (Map, empty, insert, lookup)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Upc
import Dot

data UpcConfig = UpcConfig { ucRelabel :: Maybe (Int -> String -> String) }

upcConfig0 :: UpcConfig
upcConfig0      = UpcConfig { ucRelabel = Nothing }
upcPatentConfig :: UpcConfig
upcPatentConfig = UpcConfig { ucRelabel = Just patentLabel }

patentLabel :: Int -> String -> String
patentLabel ix l = printf "S%04d: %s" (ix * 100) l

upcToDigraph :: UpcConfig -> UpcProg -> Digraph
upcToDigraph cfg (UpcProg stats) =
  let (nn, ns, lab2nix) = foldr locate (0, [], M.empty) stats
      ns'               = maybe ns (flip addStepID ns) (ucRelabel cfg)
      (_, es)           = foldr (connect lab2nix) (0, []) stats
      nstart            = Node "start" (nodeAttr0 { naLabel = Just "Start" })
      nend              = Node "end"   (nodeAttr0 { naLabel = Just "End" })
      estart            = Edge "start" (ixToNodeName (nn - 1)) edgeAttr0
  in Digraph "flowchart" [] (nstart : ns' ++ [nend]) (estart : es) digraphAttr0

addStepID :: (Int -> String -> String) -> [Node] -> [Node]
addStepID f ns =
  [Node n (a {naLabel = Just (f ix l)})
    | (Node n a@(NodeAttr {naLabel = Just l}), ix) <- zip ns [0 ..]]

locate :: Stat -> (Int, [Node], Map String String) -> (Int, [Node], Map String String)
locate s@(SBranch p _ _) (ix, ns, m) =
  let a = nodeAttr0 { naLabel = Just p, naShape = stmtShape s }
      n = Node (ixToNodeName ix) a
  in (ix + 1, n : ns, m)
locate (SLabel l) (ix, ns, m) =
  let m' = M.insert l topLabel m
  in (ix, ns, m')
  where
    topLabel | (Node _ NodeAttr {naLabel = Just s} : _) <- ns = s
             | []                                       <- ns = "end"
             | otherwise                                      = error $ "topLabel: " ++ show ns

connect :: M.Map String String -> Stat -> (Int, [Edge]) -> (Int, [Edge])
connect _    (SLabel _)        acc      = acc
connect labs (SBranch _ yl nl) (ix, es) =
  let here   = ixToNodeName ix
      mey    = fmap (\l -> Edge here (nodeName l) edgeAttr0{ eaLabel = Just "Yes" }) yl
      men    = fmap (\l -> Edge here (nodeName l) edgeAttr0{ eaLabel = Just "No"  }) nl
      next l = Edge here nextNodeName edgeAttr0{ eaLabel = l }
  in case (mey, men) of
      (Nothing, Nothing) -> (ix + 1, next Nothing                         : es)
      (Just ey, Nothing) -> (ix + 1, ey                : next (Just "No") : es)
      (Nothing, Just en) -> (ix + 1, next (Just "Yes") : en               : es)
      (Just ey, Just en) -> (ix + 1, ey                : en               : es)
  where
    nodeName l = fromMaybe (error $ "connect: label \"" ++ l ++ "\" not found") $ M.lookup l labs
    nextNodeName | Edge src _ _ : _ <- es = src
                 | otherwise              = "end"

ixToNodeName :: Int -> String
ixToNodeName = ("n" ++) . show

stmtShape :: Stat -> Maybe Shape
stmtShape (SBranch _ Nothing Nothing) = Just Box
stmtShape (SBranch _ _       _      ) = Just Diamond
stmtShape (SLabel _)                  = Nothing

