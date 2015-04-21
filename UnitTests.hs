import Test.HUnit
import System.Exit (exitFailure)
import OrderedJobsKata (thing)

testTest1 = TestCase $ do
    assertEqual "thing is correct" "Hello from OrderedJobsKata" thing

tests = TestList [
        TestLabel "testTest1" testTest1
    ]

main :: IO ()
main =
    do
        counts <- runTestTT tests
        if errors counts > 0 || failures counts > 0
            then exitFailure
            else return ()
