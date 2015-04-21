module OrderedJobsKata (orderJobs) where

import Data.List.Split (wordsBy)

orderJobs :: String -> String
orderJobs "" = ""
orderJobs s =
    concat $ map (!!0) js2
    where
        js1 = lines s
        js2 = map (wordsBy (==' ')) js1
