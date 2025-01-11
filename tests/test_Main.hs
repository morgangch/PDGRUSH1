module TestMain (testIsOperator, testParseArgs, testMyReadMaybe, testParseInts, testHasInvalidOp, testHasInvalidInt) where
import Test.HUnit
import Main (isOperator, parseArgs, myReadMaybe, parseInts, hasInvalidOp, hasInvalidInt)

testIsOperator = TestList [
    TestCase (assertBool "for (isOperator \"sa\")," (isOperator "sa")),
    TestCase (assertBool "for (isOperator \"sb\")," (isOperator "sb")),
    TestCase (assertBool "for (isOperator \"sc\")," (isOperator "sc")),
    TestCase (assertBool "for (isOperator \"pa\")," (isOperator "pa")),
    TestCase (assertBool "for (isOperator \"pb\")," (isOperator "pb")),
    TestCase (assertBool "for (isOperator \"ra\")," (isOperator "ra")),
    TestCase (assertBool "for (isOperator \"rb\")," (isOperator "rb")),
    TestCase (assertBool "for (isOperator \"rr\")," (isOperator "rr")),
    TestCase (assertBool "for (isOperator \"rra\")," (isOperator "rra")),
    TestCase (assertBool "for (isOperator \"rrb\")," (isOperator "rrb")),
    TestCase (assertBool "for (isOperator \"rrr\")," (isOperator "rrr")),

    TestCase (assertBool "for (isOperator \" \")," (not (isOperator " "))),
    TestCase (assertBool "for (isOperator \"invalid\")," (not (isOperator "invalid")))
    ]

testParseArgs = TestList [
    TestCase (assertEqual "for (parseArgs \"sa sb\")," (Just ["sa", "sb"]) (parseArgs "sa sb")),
    TestCase (assertEqual "for (parseArgs \"sa invalid\")," (Just ["sa", "IP"]) (parseArgs "sa invalid")),
    TestCase (assertEqual "for (parseArgs \"\")," (Just []) (parseArgs "")),
    TestCase (assertEqual "for (parseArgs \"invalid\")," (Just ["IP"]) (parseArgs "invalid"))
    ]

testMyReadMaybe = TestList [
    TestCase (assertEqual "for (myReadMaybe \"123\")," 123 (myReadMaybe "123")),
    TestCase (assertEqual "for (myReadMaybe \"abc\")," (-1) (myReadMaybe "abc"))
    ]

testParseInts = TestList [
    TestCase (assertEqual "for (parseInts [\"123\", \"456\"])," [123, 456] (parseInts ["123", "456"])),
    TestCase (assertEqual "for (parseInts [\"123\", \"abc\"])," [123, -1] (parseInts ["123", "abc"]))
    ]

testHasInvalidOp = TestList [
    TestCase (assertBool "for (hasInvalidOp [\"sa\", \"sb\"])," (not (hasInvalidOp ["sa", "sb"]))),
    TestCase (assertBool "for (hasInvalidOp [\"sa\", \"IP\"])," (hasInvalidOp ["sa", "IP"]))
    ]

testHasInvalidInt = TestList [
    TestCase (assertBool "for (hasInvalidInt [123, 456])," (not (hasInvalidInt [123, 456]))),
    TestCase (assertBool "for (hasInvalidInt [123, -1])," (hasInvalidInt [123, -1]))
    ]
