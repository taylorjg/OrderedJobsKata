import Test.HUnit
import System.Exit (exitFailure)
import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)
import OrderedJobsKata

testEmptyString = TestCase $ do
    let input = ""
    assertEqual "unexpected output" "" $ orderJobs input

testSingleJob = TestCase $ do
    let input = "a =>"
    let output = orderJobs input
    assertEqual "unexpected output" "a" $ orderJobs input

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

testMultipleJobsSingleDependency = TestCase $ do
    let a = "a =>"
    let b = "b => c"
    let c = "c =>"
    let input = unlines [a,b,c]
    let output = orderJobs input
    let mb = findIndex (=='b') output
    let mc = findIndex (=='c') output
    if isJust mb && isJust mc then
        let
            idxb = fromJust mb
            idxc = fromJust mc
        in
            assertBool "expected c to appear before b" $ idxc < idxb
    else
        assertFailure "expected output to contain b and c"

tests = TestList [
        TestLabel "testEmptyString" testEmptyString,
        TestLabel "testSingleJob" testSingleJob,
        TestLabel "testMultipleJobs" testMultipleJobs,
        TestLabel "testMultipleJobsSingleDependency" testMultipleJobsSingleDependency
    ]

main :: IO ()
main =
    do
        counts <- runTestTT tests
        if errors counts > 0 || failures counts > 0
            then exitFailure
            else return ()
