module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)
import Data.List (sortBy, partition)

data Job
    = Single { job :: String }
    | Pair { job :: String, dependency :: String }

orderJobs :: String -> Maybe String
orderJobs [] = Just []
orderJobs input =
    if pairsContainCycle pairs
        then Nothing
        else Just orderedLetters
    where
        jobLines = lines input
        (singles, pairs) = parseJobLines jobLines
        orderedPairs = orderPairs pairs
        orderedLetters = concat $ map job (singles ++ orderedPairs)

parseJobLines :: [String] -> ([Job], [Job])
parseJobLines jobLines =
    (singles, pairs)
    where
        jobLinesSplit = map (wordsBy (==' ')) jobLines
        listsOf2 = filter (\xs -> length xs == 2) jobLinesSplit
        listsOf3 = filter (\xs -> length xs == 3) jobLinesSplit
        singles = map (Single . head) listsOf2
        pairs = map (\xs -> Pair (head xs) (last xs)) listsOf3

orderPairs :: [Job] -> [Job]
orderPairs ps =
    loop ps []
    where
        loop [] acc = acc
        loop ps acc =
            loop as (acc ++ bs)
            where
                jobs = map job ps
                (as, bs) = partition (\p -> (dependency p) `elem` jobs) ps

pairsContainCycle :: [Job] -> Bool
pairsContainCycle ps =
    or $ map (checkDependencyForCycle ps) dependencies
    where
        dependencies = (map dependency ps)

checkDependencyForCycle :: [Job] -> String -> Bool
checkDependencyForCycle ps d =
    loop d []
    where
        loop d seenList =
            case dependencies of
                [] -> False
                d':_ -> d' `elem` seenList' || loop d' seenList'
            where
                seenList' = d:seenList
                dependencies = map dependency (filter (\p -> job p == d) ps)
