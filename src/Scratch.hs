module Scratch where

import Data.List (nub)

import qualified Data.Set as S
import qualified Data.Dequeue as Q

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
        search (v:vs) visited poped
            | v == toV = poped ++ [v]
            | otherwise =
                search
                    (vs ++ nonVisitedReachables visited v)
                    (S.insert v visited)
                    (nub $ poped ++ [v])

        nonVisitedReachables visited v = filter
            (\v' -> not $ S.member v' visited) (directlyReachableVertices g v)

path' :: UGraph Int () -> Int -> Int -> [Int]
path' g fromV toV
    | fromV == toV = [toV]
    | otherwise = undefined
    -- | otherwise = search (Q.fromList [fromV]) S.empty Q.empty
    where
        search :: Q.BankersDequeue Int -> S.Set Int -> Q.BankersDequeue Int -> Q.BankersDequeue Int
        search = undefined
        -- search (v:vs) visited poped
        --     | v == toV = poped ++ [v]
        --     | otherwise =
        --         search
        --             (vs ++ nonVisitedReachables visited v)
        --             (S.insert v visited)
        --             (nub $ poped ++ [v])

        -- nonVisitedReachables visited v = filter
        --     (\v' -> not $ S.member v' visited) (directlyReachableVertices g v)
