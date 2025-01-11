module Test where
import Test.HUnit
import TestMain (testIsOperator, testParseArgs, testMyReadMaybe, testParseInts, testHasInvalidOp, testHasInvalidInt)
import TestCChecker (testIsSorted, testDoOperation)
import TestUtilities (testSwap, testSwapb, testPa, testPb, testRotate, testRr, testRotaterev, testRrr)

main :: IO ()
main = do
    runTestTT $ TestList [
        testIsOperator,
        testParseArgs,
        testMyReadMaybe,
        testParseInts,
        testHasInvalidOp,
        testHasInvalidInt,
        testIsSorted,
        testDoOperation,
        testSwap,
        testSwapb,
        testPa,
        testPb,
        testRotate,
        testRr,
        testRotaterev,
        testRrr
        ]
    return ()