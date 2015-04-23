module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)
import Data.List (sortBy)

orderJobs :: String -> String
orderJobs [] = []
orderJobs s =
    concat $ map head js3
    where
        js1 = lines s
        js2 = map (wordsBy (==' ')) js1
        js3 = sortBy sortByLength js2
        sortByLength a b
            | length a < length b = LT
            | length a == length b = EQ
            | length a > length b = GT
