module UpcToDot where

import Data.Map as M (Map, empty, insert, lookup)
import Text.Printf (printf)
import Upc
import Dot

data UpcConfig = UpcConfig { ucRelabel :: Maybe (Int -> String -> String) }

upcConfig0 :: UpcConfig
upcConfig0      = UpcConfig { ucRelabel = Nothing }
upcPatentConfig :: UpcConfig
upcPatentConfig = UpcConfig { ucRelabel = Just patentLabel }

patentLabel :: Int -> String -> String
patentLabel ix l = printf "S%05d: %s" ix l

upcToDigraph :: UpcConfig -> Upc -> Digraph
upcToDigraph cfg (Upc steps) =
  let (ns, lab2nix) = foldr locate ([], M.empty)     $ zip steps [0 ..]
      ns'           = maybe ns (flip addStepID ns) (ucRelabel cfg)
      es            = concat $ map (connect lab2nix) $ zip steps [0 ..]
      nstart        = Node "start" (nodeAttr0 { naLabel = Just "Start" })
      nend          = Node (ixToNodeName (length steps)) (nodeAttr0 { naLabel = Just "End" })
      estart        = Edge "start" (ixToNodeName 0) edgeAttr0
  in Digraph "flowchart" [] (nstart : ns ++ [nend]) (estart : es) digraphAttr0

addStepID :: (Int -> String -> String) -> [Node] -> [Node]
addStepID f ns =
  [Node n (a {naLabel = Just (f ix l)})
    | (Node n a@(NodeAttr {naLabel = Just l}), ix) <- zip ns [0 ..]]

locate :: (Step, Int) -> ([Node], Map String Int) -> ([Node], Map String Int)
locate (Step lab st, ix) (ns, m) =
  let m' = maybe m (\k -> M.insert k ix m) lab
      a  = nodeAttr0 { naLabel = Just (stmtStr st), naShape = stmtShape st }
      n  = Node (ixToNodeName ix) a
  in (n : ns, m')

connect :: M.Map String Int -> (Step, Int) -> [Edge]
connect lab2nix (Step _ (Statement yes no act), ix) =
  let here    = ixToNodeName ix
      mey     = fmap (\l -> Edge here (node_name l) (edgeAttr0 { eaLabel = Just "Yes" })) yes
      men     = fmap (\l -> Edge here (node_name l) (edgeAttr0 { eaLabel = Just "No"  })) no
      next yn = Edge here (ixToNodeName $ ix + 1) (edgeAttr0 { eaLabel = yn })
  in case (mey, men) of
      (Nothing, Nothing) -> [next Nothing] 
      (Just ey, Nothing) -> [ey,                next (Just "No")]
      (Nothing, Just en) -> [next (Just "Yes"), en              ]
      (Just ey, Just en) -> [ey,                en              ]
  where
    node_name l = ixToNodeName $ lookupStepIx l lab2nix

lookupStepIx :: String -> M.Map String Int -> Int
lookupStepIx lab m =
  case M.lookup lab m of
    Nothing -> error $ "Label \"" ++ lab ++ "\" not found"
    Just ix -> ix

ixToNodeName :: Int -> String
ixToNodeName = ("n" ++) . show

stmtStr :: Statement -> String
stmtStr (Statement _ _ s) = s

stmtShape :: Statement -> Maybe Shape
stmtShape (Statement Nothing Nothing _) = Just Box
stmtShape (Statement _       _       _) = Just Diamond

