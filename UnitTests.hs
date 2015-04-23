import Test.HUnit
import System.Exit (exitFailure)
import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)
import OrderedJobsKata

testEmptyString = TestCase $ do
    let input = ""
    let output = orderJobs input
    assertOutputContains output "" 

testSingleJob = TestCase $ do
    let input = "a =>"
    let output = orderJobs input
    assertOutputContains output "a"

testMultipleJobs = TestCase $ do
    let a = "a =>"
    let b = "b =>"
    let c = "c =>"
    let input = unlines [a,b,c]
    let output = orderJobs input
    assertOutputContains output "abc"

testMultipleJobsSingleDependency = TestCase $ do
    let a = "a =>"
    let b = "b => c"
    let c = "c =>"
    let input = unlines [a,b,c]
    let output = orderJobs input
    assertOutputContains output "acb"
    assertCharAppearsBeforeChar output 'c' 'b'

testMultipleJobsMultipleDependencies = TestCase $ do
    let a = "a =>"
    let b = "b => c"
    let c = "c => f"
    let d = "d => a"
    let e = "e => b"
    let f = "f =>"
    let input = unlines [a,b,c,d,e,f]
    let output = orderJobs input
    assertOutputContains output "abcdef"
    assertCharAppearsBeforeChar output 'f' 'c'
    assertCharAppearsBeforeChar output 'c' 'b'
    assertCharAppearsBeforeChar output 'b' 'e'
    assertCharAppearsBeforeChar output 'a' 'd'

assertOutputContains actualOutput chars =
    let
        actualOutputLength = length actualOutput
        charsLength = length chars
        msg1 = "expected length of '" ++ actualOutput ++ "' to be the same as the length of '" ++ chars ++ "'"
        msg2 = "expected all elements of '" ++ chars ++ "' to be present in '" ++ actualOutput ++ "'"
    in
        do
            assertEqual msg1 charsLength actualOutputLength
            assertBool msg2 $ all (flip elem actualOutput) chars

assertCharAppearsBeforeChar actualOutput c1 c2  =
    if isJust mc1 && isJust mc2 then
        let
            idxc1 = fromJust mc1
            idxc2 = fromJust mc2
        in
            assertBool msg1 (idxc1 < idxc2)
    else
        assertFailure msg2
    where
        mc1 = findIndex (==c1) actualOutput
        mc2 = findIndex (==c2) actualOutput
        msg1 = "expected " ++ show c1 ++ " to appear before " ++ show c2 ++ " in '" ++ actualOutput ++ "'"
        msg2 = "expected '" ++ actualOutput ++ "' to contain " ++ show c1 ++ " and " ++ show c2

tests = TestList [
        TestLabel "testEmptyString" testEmptyString,
        TestLabel "testSingleJob" testSingleJob,
        TestLabel "testMultipleJobs" testMultipleJobs,
        TestLabel "testMultipleJobsSingleDependency" testMultipleJobsSingleDependency,
        TestLabel "testMultipleJobsMultipleDependencies" testMultipleJobsMultipleDependencies
    ]

main :: IO ()
main =
    do
        counts <- runTestTT tests
        if errors counts > 0 || failures counts > 0
            then exitFailure
            else return ()
