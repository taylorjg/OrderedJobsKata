import Test.HUnit
import System.Exit (exitFailure)
import OrderedJobsKata

testEmptyString = TestCase $ do
    assertEqual "unexpected output" "" $ orderJobs ""

testSingleJob = TestCase $ do
    assertEqual "unexpected output" "a" $ orderJobs "a =>"

testMultipleJobs = TestCase $ do
    let a = "a =>"
    let b = "b =>"
    let c = "c =>"
    let input = unlines [a,b,c]
    let output = orderJobs input
    assertBool "expected output to contain a" $ 'a' `elem` output
    assertBool "expected output to contain b" $ 'b' `elem` output
    assertBool "expected output to contain c" $ 'c' `elem` output
    assertEqual "expected output to have a length of 3" 3 $ length output

tests = TestList [
        TestLabel "testEmptyString" testEmptyString,
        TestLabel "testSingleJob" testSingleJob,
        TestLabel "testMultipleJobs" testMultipleJobs
    ]

main :: IO ()
main =
    do
        counts <- runTestTT tests
        if errors counts > 0 || failures counts > 0
            then exitFailure
            else return ()
