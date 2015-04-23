module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)
import Data.List (sortBy, partition)

orderJobs :: String -> String
orderJobs [] = []
orderJobs s =
    sortedletters
    where
        jobLines = lines s
        jobLinesSplit = map (wordsBy (==' ')) jobLines
        listsOf2 = filter (\xs -> length xs == 2) jobLinesSplit
        listsOf3 = filter (\xs -> length xs == 3) jobLinesSplit
        singles = map head listsOf2
        pairs = map (\xs -> (head xs, last xs)) listsOf3
        sortedPairs = sortPairs pairs []
        firstsOfSortedPairs = map fst sortedPairs
        sortedletters = concat $ singles ++ firstsOfSortedPairs
        sortPairs [] acc = acc
        sortPairs ps acc =
            let
                firsts = map fst ps
                (as, bs) = partition (\p -> (snd p) `elem` firsts) ps
            in
                sortPairs as (acc ++ bs)
