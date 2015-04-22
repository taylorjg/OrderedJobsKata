import OrderedJobsKata (orderJobs)

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
        putStr $ "Enter jobs (blank line to end input): "
        lines <- getLines
        let input = unlines lines
        let output = orderJobs input
        putStrLn $ "Ordered jobs: " ++ output
