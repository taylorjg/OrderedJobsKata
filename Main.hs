import OrderedJobsKata (orderJobs)

main :: IO ()
main =
    do
        putStr $ "Enter jobs: "
        input <- getLine
        putStrLn $ "You entered: " ++ input
        let output = orderJobs input
        putStrLn $ "Ordered jobs: " ++ output
