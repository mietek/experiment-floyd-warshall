module Graph
  ( module Matrix
  , Graph
  , LabeledGraph
  , graph
  , labeledGraph
  , labeledGraph'
  , labelWithEdge
  , unlabel
  , regExpMap
  ) where

import Control.Monad (mplus)

import Connection
import Matrix
import RegExp


type Graph i =
    Matrix i Connection


type LabeledGraph i a =
    Matrix i (Maybe a)


type RegExpGraph i a =
    Matrix i (RegExp a)


graph :: (Ix i, Bounded i) => [Edge i] -> Graph i
graph edgeList =
    matrix build
  where
    build i
      | i `elem` edgeList = Connected
      | otherwise         = Unconnected


labeledGraph :: (Ix i, Bounded i) => [(Edge i, a)] -> LabeledGraph i a
labeledGraph edgeList =
    matrix build
  where
    build (i :-> j) =
        lookup (i :-> j) edgeList `mplus` lookup (j :-> i) edgeList


labeledGraph' :: (Ix i, Bounded i) => [(i, [(i, a)])] -> LabeledGraph i a
labeledGraph' adjacencyList =
    labeledGraph edgeList
  where
    edgeList = [ (node1 :-> node2, edgeLabel)
               | (node1, adjacentEdges) <- adjacencyList
               , (node2, edgeLabel) <- adjacentEdges
               ]


labelWithEdge :: (Ix i, Bounded i) => Graph i -> LabeledGraph i (Edge i)
labelWithEdge m =
    f <$> m <*> matrix id
  where
    f Connected   l = Just l
    f Unconnected _ = Nothing


unlabel :: (Ix i) => LabeledGraph i a -> Graph i
unlabel =
    fmap (maybe Unconnected (const Connected))


regExpMap :: (Ix i) => LabeledGraph i a -> RegExpGraph i a
regExpMap =
    fmap (maybe zero regExp)
