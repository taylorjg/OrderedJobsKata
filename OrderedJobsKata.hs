module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)
import Data.List (sortBy, partition)

orderJobs :: String -> String
orderJobs [] = []
orderJobs s =
    concat letters
    where
        jsLines = lines s
        jsLinesInBits = map (wordsBy (==' ')) jsLines
        listsOf2 = filter (\xs -> length xs == 2) jsLinesInBits
        listsOf3 = filter (\xs -> length xs == 3) jsLinesInBits
        singles = map head listsOf2
        pairs = map (\xs -> (head xs, last xs)) listsOf3
        sortedPairs = sortPairs pairs []
        letters = singles ++ (map fst sortedPairs)
        sortPairs [] acc = acc
        sortPairs ps acc =
            let
                firsts = map fst ps
                (as, bs) = partition (\p -> (snd p) `elem` firsts) ps
            in
                sortPairs as (acc ++ bs)
