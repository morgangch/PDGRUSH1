module TestCChecker (testIsSorted, testDoOperation) where
import Test.HUnit
import CommandChecker (isSorted, doOperation)
import Utilities (swap, swapb, pa, pb, rotate, rr, rotaterev, rrr)

-- Test cases for isSorted
testIsSorted = TestList [
    TestCase (assertBool "Empty list is sorted" (isSorted [])),
    TestCase (assertBool "Single element list is sorted" (isSorted [1])),
    TestCase (assertBool "Sorted list is sorted" (isSorted [1, 2, 3])),
    TestCase (assertBool "Unsorted list is not sorted" (not (isSorted [3, 2, 1])))
    ]

-- Test cases for doOperation
testDoOperation = TestList [
    TestCase (assertEqual "sa operation" ([2, 1, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "sa")),
    TestCase (assertEqual "sa operation with not enough elements" ([2], []) (doOperation ([2], []) "sa")),
    TestCase (assertEqual "sa operation with empty elements list" ([], []) (doOperation ([], []) "sa")),
    
    TestCase (assertEqual "sb operation" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "sb")),
    TestCase (assertEqual "sb operation with not enough elements" ([2], []) (doOperation ([2], []) "sb")),
    TestCase (assertEqual "sb operation with empty elements list" 
        ([], []) (doOperation ([], []) "sb")),
    
    TestCase (assertEqual "sc operation" ([2, 1, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "sc")),
    TestCase (assertEqual "sc operation with not enough elements" ([2], []) (doOperation ([2], []) "sc")),
    TestCase (assertEqual "sc operation with empty elements list" ([], []) (doOperation ([], []) "sc")),
    
    TestCase (assertEqual "pa operation" ([4, 1, 2, 3], [5]) (doOperation ([1, 2, 3], [4, 5]) "pa")),
    TestCase (assertEqual "pa operation with few l_a elements" ([2], []) (doOperation ([2], []) "pa")),
    TestCase (assertEqual "pa operation with few l_b elements" ([2], []) (doOperation ([], [2]) "pa")),
    TestCase (assertEqual "pa operation with empty elements list" ([], []) (doOperation ([], []) "pa")),
    
    TestCase (assertEqual "pb operation" ([2, 3], [1, 4, 5]) (doOperation ([1, 2, 3], [4, 5]) "pb")),
    TestCase (assertEqual "pb operation with few l_a elements" ([], [2]) (doOperation ([2], []) "pb")),
    TestCase (assertEqual "pb operation with few l_b elements" ([], [2]) (doOperation ([], [2]) "pb")),
    TestCase (assertEqual "pb operation with empty elements list" ([], []) (doOperation ([], []) "pb")),
    
    TestCase (assertEqual "ra operation" ([2, 3, 1], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "ra")),
    TestCase (assertEqual "ra operation with not enough elements" ([2, 3, 1], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "ra")),
    TestCase (assertEqual "ra operation with empty elements list" ([2, 3, 1], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "ra")),
    
    TestCase (assertEqual "rb operation" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rb")),
    TestCase (assertEqual "rb operation with not enough elements" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rb")),
    TestCase (assertEqual "rb operation with empty elements list" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rb")),
    
    TestCase (assertEqual "rr operation" ([2, 3, 1], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rr")),
    TestCase (assertEqual "rr operation with not enough elements" ([2, 3, 1], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rr")),
    TestCase (assertEqual "rr operation with empty elements list" ([2, 3, 1], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rr")),
    
    TestCase (assertEqual "rra operation" ([3, 1, 2], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "rra")),
    TestCase (assertEqual "rra operation with not enough elements" ([3, 1, 2], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "rra")),
    TestCase (assertEqual "rra operation with empty elements list" ([3, 1, 2], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "rra")),
    
    TestCase (assertEqual "rrb operation" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rrb")),
    TestCase (assertEqual "rrb operation with not enough elements" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rrb")),
    TestCase (assertEqual "rrb operation with empty elements list" ([1, 2, 3], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rrb")),
    
    TestCase (assertEqual "rrr operation" ([3, 1, 2], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rrr")),
    TestCase (assertEqual "rrr operation with not enough elements" ([3, 1, 2], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rrr")),
    TestCase (assertEqual "rrr operation with empty elements list" ([3, 1, 2], [5, 4]) (doOperation ([1, 2, 3], [4, 5]) "rrr")),
    
    TestCase (assertEqual "Unknown operation" ([1, 2, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "unknown"))
    ]
