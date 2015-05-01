module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)
import Data.List (sortBy, partition)
import Text.Regex.Posix

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
        css = map parseJobLine jobLines
        jobs = makeJobs css
        (singles, pairs) = partition isSingle jobs
        isSingle job = case job of
            Single _ -> True
            _ -> False

parseJobLine :: String -> [String]
parseJobLine jobLine = 
    captures
    where
        jobPattern = "([a-z])[ \t]*=>[ \t]*([a-z])?"
        (_, _, _, captures) = jobLine =~ jobPattern :: (String,String,String,[String])

makeJobs :: [[String]] -> [Job]
makeJobs css =
    map makeJob css
    where
        makeJob cs =
            if (length cs == 1)
                then Single (head cs)
                else Pair (head cs) (last cs)

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
