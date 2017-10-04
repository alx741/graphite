module Scratch where

import Data.List (foldl')

import qualified Data.Dequeue as Q
import qualified Data.Set     as S

import Data.Graph.Types
import Data.Graph.UGraph

testG :: UGraph Int ()
testG = fromList
    [ 1 <-> 2
    , 1 <-> 3
    , 1 <-> 5
    , 2 <-> 1
    , 2 <-> 4
    , 3 <-> 4
    , 3 <-> 6
    , 4 <-> 2
    , 4 <-> 3
    , 4 <-> 5
    , 5 <-> 1
    , 5 <-> 4
    , 5 <-> 6
    , 6 <-> 5
    , 6 <-> 3
    ]

path :: UGraph Int () -> Int -> Int -> [Int]
path g fromV toV
    | fromV == toV = [toV]
    | otherwise = search [fromV] S.empty []
    where
        search :: [Int] -> S.Set Int -> [Int] -> [Int]
        search (v:vs) banned popped
            | v == toV = popped ++ [v]
            | otherwise =
                let reachables = nonVisitedReachables banned v
                in search
                    (vs ++ reachables)
                    (setInsertMany banned $ v : reachables)
                    (popped ++ [v])

        nonVisitedReachables banned v = filter
            (\v' -> v' /= v && (not $ S.member v' banned))
            (directlyReachableVertices g v)

path' :: UGraph Int () -> Int -> Int -> [Int]
path' g fromV toV
    | fromV == toV = [toV]
    | otherwise = reverse $ search (Q.fromList [fromV]) S.empty []
    where
        search :: Q.BankersDequeue Int -> S.Set Int -> [Int] -> [Int]
        search queue banned popped = case Q.popFront queue of
            Nothing -> popped
            Just (v, queue') -> if v == toV then v : popped else
                let reachables = nonVisitedReachables banned v
                in search
                    (queue' `pushBackMany` reachables)
                    (setInsertMany banned $ v : reachables)
                    (v : popped)

        nonVisitedReachables banned v = filter
            (\v' -> v' /= v && (not $ S.member v' banned))
            (directlyReachableVertices g v)


setInsertMany :: Ord a => S.Set a -> [a] -> S.Set a
setInsertMany = foldl' (flip S.insert)

pushBackMany :: Q.BankersDequeue a  -> [a] -> Q.BankersDequeue a
pushBackMany = foldl' Q.pushBack
