import OrderedJobsKata (orderJobs)

main :: IO ()
main =
    do
        putStr $ "Enter jobs: "
        -- TODO: allow multiple lines to be entered - loop until a blank line is seen
        input <- getLine
        let output = orderJobs input
        putStrLn $ "Ordered jobs: " ++ output
