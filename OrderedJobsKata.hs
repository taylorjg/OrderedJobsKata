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
            let
                seconds = map snd ps
                cycleFlags = map findCycle seconds
                findCycle x =
                    loop x []
                    where
                        loop x seenList =
                            let
                                seenList' = x:seenList
                                ps' = filter (\p -> fst p == x) ps
                                seconds' = map snd ps'
                            in
                                case seconds' of
                                    [] -> False
                                    y:_ -> y `elem` seenList' || loop y seenList'
            in 
                or cycleFlags
