module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)
import Data.List (sortBy, partition)

orderJobs :: String -> Maybe String
orderJobs [] = Just []
orderJobs s =
    if pairsContainCycle pairs
        then Nothing
        else Just sortedletters
    where
        jobLines = lines s
        jobLinesSplit = map (wordsBy (==' ')) jobLines
        listsOf2 = filter (\xs -> length xs == 2) jobLinesSplit
        listsOf3 = filter (\xs -> length xs == 3) jobLinesSplit
        singles = map head listsOf2
        pairs = map (\xs -> (head xs, last xs)) listsOf3
        sortedPairs = sortPairs pairs
        firstsOfSortedPairs = map fst sortedPairs
        sortedletters = concat $ singles ++ firstsOfSortedPairs

sortPairs ps =
    loop ps []
    where
        loop [] acc = acc
        loop ps acc =
            loop as (acc ++ bs)
            where
                firsts = map fst ps
                (as, bs) = partition (\p -> (snd p) `elem` firsts) ps

pairsContainCycle ps =
    or $ map (\s -> checkForCycle s ps) (map snd ps)

checkForCycle s ps =
    loop s []
    where
        loop s seenList =
            case snds of
                [] -> False
                s':_ -> s' `elem` seenList' || loop s' seenList'
            where
                seenList' = s:seenList
                snds = map snd (filter (\p -> fst p == s) ps)
