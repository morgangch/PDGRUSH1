module TestUtilities (testSwap, testSwapb, testPa, testPb, testRotate, testRr, testRotaterev, testRrr) where
import Test.HUnit
import Utilities

testSwap :: Test
testSwap = TestList [
    "swap empty list" ~: swap ([] :: [Int]) ~?= [],
    "swap single element list" ~: swap [1] ~?= [1],
    "swap two elements list" ~: swap [1, 2] ~?= [2, 1],
    "swap multiple elements list" ~: swap [1, 2, 3, 4] ~?= [2, 1, 3, 4]
    ]

testSwapb :: Test
testSwapb = TestList [
    "swapb empty lists" ~: swapb ([] :: [Int]) ([] :: [Int]) ~?= ([], []),
    "swapb non-empty lists" ~: swapb [1, 2] [3, 4] ~?= ([2, 1], [4, 3])
    ]

testPa :: Test
testPa = TestList [
    "pa empty second list" ~: pa [1, 2] ([] :: [Int]) ~?= ([1, 2], []),
    "pa non-empty second list" ~: pa [1, 2] [3, 4] ~?= ([3, 1, 2], [4])
    ]

testPb :: Test
testPb = TestList [
    "pb empty first list" ~: pb ([] :: [Int]) [1, 2] ~?= ([], [1, 2]),
    "pb non-empty first list" ~: pb [1, 2] [3, 4] ~?= ([2], [1, 3, 4])
    ]

testRotate :: Test
testRotate = TestList [
    "rotate empty list" ~: rotate ([] :: [Int]) ~?= [],
    "rotate single element list" ~: rotate [1] ~?= [1],
    "rotate multiple elements list" ~: rotate [1, 2, 3, 4] ~?= [2, 3, 4, 1]
    ]

testRr :: Test
testRr = TestList [
    "rr empty lists" ~: rr ([] :: [Int]) ([] :: [Int]) ~?= ([], []),
    "rr non-empty lists" ~: rr [1, 2] [3, 4] ~?= ([2, 1], [4, 3])
    ]

testRotaterev :: Test
testRotaterev = TestList [
    "rotaterev empty list" ~: rotaterev ([] :: [Int]) ~?= [],
    "rotaterev single element list" ~: rotaterev [1] ~?= [1],
    "rotaterev multiple elements list" ~: rotaterev [1, 2, 3, 4] ~?= [4, 1, 2, 3]
    ]

testRrr :: Test
testRrr = TestList [
    "rrr empty lists" ~: rrr ([] :: [Int]) ([] :: [Int]) ~?= ([], []),
    "rrr non-empty lists" ~: rrr [1, 2] [3, 4] ~?= ([2, 1], [4, 3])
    ]

