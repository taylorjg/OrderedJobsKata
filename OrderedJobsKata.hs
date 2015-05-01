module OrderedJobsKata (orderJobs) where

import Data.List (partition)
import Text.Regex.Posix ((=~))

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
        jobs = parseJobLines jobLines
        (singles, pairs) = partition isSingleJob jobs
        orderedPairs = orderPairs pairs
        orderedLetters = concat $ map job (singles ++ orderedPairs)
        isSingleJob job = case job of
            Single _ -> True
            _ -> False

parseJobLines :: [String] -> [Job]
parseJobLines jobLines =
    makeJobs parsedJobLines
    where
        parsedJobLines = map parseJobLine jobLines

parseJobLine :: String -> [String]
parseJobLine jobLine = 
    captures
    where
        jobPattern = "([a-z])[ \t]*=>[ \t]*([a-z])?"
        (_, _, _, captures) = jobLine =~ jobPattern :: (String,String,String,[String])

makeJobs :: [[String]] -> [Job]
makeJobs parsedJobLines =
    map makeJob parsedJobLines
    where
        makeJob captures =
            if (length captures == 1)
                then Single (head captures)
                else Pair (head captures) (last captures)

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
