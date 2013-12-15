module Dot where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Data.Char (toLower)

data Digraph = Digraph String [Subgraph] [Node] [Edge] DigraphAttr
  deriving (Show)

data DigraphAttr = DigraphAttr ()
  deriving (Show)

data Subgraph = Subgraph () -- TODO: not implemented
  deriving (Show)

data Node = Node String NodeAttr
  deriving (Show)

data NodeAttr = NodeAttr { naLabel :: Maybe String
                         , naShape :: Maybe Shape }
  deriving (Show)

data Shape = Box | Diamond | Ellipse
  deriving (Show)

data Edge = Edge String String EdgeAttr
  deriving (Show)

data EdgeAttr = EdgeAttr { eaLabel :: Maybe String }
  deriving (Show)

digraphAttr0 :: DigraphAttr
digraphAttr0 = DigraphAttr ()

nodeAttr0 :: NodeAttr
nodeAttr0 = NodeAttr Nothing Nothing

edgeAttr0 :: EdgeAttr
edgeAttr0 = EdgeAttr Nothing

digraphToStr :: Digraph -> String
digraphToStr (Digraph name _ ns es _) =
  concat $ intersperse "\n" $
    ["digraph " ++ name ++ " {"]
    ++ ["  " ++ nodeToStr n ++ ";" | n <- ns]
    ++ ["  " ++ edgeToStr e ++ ";" | e <- es]
    ++ ["}"]

nodeToStr :: Node -> String
nodeToStr (Node name attr) =
  name ++ " " ++ nodeAttrToStr attr

nodeAttrToStr :: NodeAttr -> String
nodeAttrToStr attr =
  let ms = [ fmap (\s -> "label = \"" ++ s ++ "\"") (naLabel attr)
           , fmap (\s -> "shape = " ++ shapeStr s)  (naShape attr) ]
  in case catMaybes ms of
      [] -> ""
      as -> "[ " ++ concat (intersperse ", " as) ++ " ]"

edgeToStr :: Edge -> String
edgeToStr (Edge src dst attr) =
  src ++ " -> " ++ dst ++ " " ++ edgeAttrToStr attr

edgeAttrToStr :: EdgeAttr -> String
edgeAttrToStr attr =
  let ms = [ fmap ("label = " ++) (eaLabel attr) ]
  in case catMaybes ms of
      [] -> ""
      as -> "[ " ++ concat (intersperse ", " as) ++ " ]"

shapeStr :: Shape -> String
shapeStr = map toLower . show

