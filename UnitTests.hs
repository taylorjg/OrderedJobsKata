import Test.HUnit
import System.Exit (exitFailure)
import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)
import OrderedJobsKata

testEmptyString = TestCase $ do
    let input = ""
    let output = orderJobs input
    
    assertExpectedOutput "" output

testSingleJob = TestCase $ do
    let input = "a =>"
    let output = orderJobs input

    assertExpectedOutput "a" output

testMultipleJobs = TestCase $ do
    let a = "a =>"
    let b = "b =>"
    let c = "c =>"
    let input = unlines [a,b,c]
    let output = orderJobs input

    assertExpectedOutput "abc" output

testMultipleJobsSingleDependency = TestCase $ do
    let a = "a =>"
    let b = "b => c"
    let c = "c =>"
    let input = unlines [a,b,c]
    let output = orderJobs input

    assertExpectedOutput "acb" output

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

assertExpectedOutput expectedOutput actualOutput =
    let
        expectedLength = length expectedOutput
        actualLength = length actualOutput
        msg1 = "expected length of '" ++ actualOutput ++ "' to be the same as the length of '" ++ expectedOutput ++ "'"
        msg2 = "expected all elements of '" ++ expectedOutput ++ "' to be present in '" ++ actualOutput ++ "'"
    in
        do
            assertEqual msg1 expectedLength actualLength
            assertBool msg2 $ all (flip elem actualOutput) expectedOutput

main :: IO ()
main =
    do
        counts <- runTestTT tests
        if errors counts > 0 || failures counts > 0
            then exitFailure
            else return ()
