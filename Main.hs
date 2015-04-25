import OrderedJobsKata (orderJobs)
import Data.Maybe (fromJust)

getLines :: IO [String]
getLines = loop []
    where
        loop lines =
            do
                line <- getLine
                if line == ""
                    then return $ reverse lines
                    else loop $ line:lines

main :: IO ()
main =
    do
        putStrLn $ "Enter jobs (blank line to end input): "
        lines <- getLines
        let input = unlines lines
        let output = fromJust $ orderJobs input
        putStrLn $ "Ordered jobs: " ++ output
