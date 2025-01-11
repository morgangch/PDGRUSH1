module TestMain (testIsOperator, testParseArgs, testParseInts, testHasInvalidOp, testHasInvalidInt) where
import Test.HUnit
import Main (isOperator, parseArgs, parseInts, hasInvalidOp, hasInvalidInt)

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
    TestCase (assertEqual "for (parseArgs \"sa sb\")," (Just ["sa", "sb"]) (parseArgs ["sa", "sb"])),
    TestCase (assertEqual "for (parseArgs \"sa invalid\")," (Just ["sa", "IP"]) (parseArgs ["sa", "invalid"])),
    TestCase (assertEqual "for (parseArgs \"\")," (Just ["IP"]) (parseArgs [""])),
    TestCase (assertEqual "for (parseArgs \"invalid\")," (Just ["IP"]) (parseArgs ["invalid"])),
    TestCase (assertEqual "for (parseArgs \"su invalid\")," (Just ["sa", "IP"]) (parseArgs ["sa", "su"])),
    TestCase (assertEqual "for (parseArgs \"sa rrr\") (three characters operators)," (Just ["sa", "rrr"]) (parseArgs ["sa", "rrr"]))
    ]

testParseInts = TestList [
    TestCase (assertEqual "for (parseInts [\"123\", \"456\"])," [Just 123, Just 456] (parseInts ["123", "456"])),
    TestCase (assertEqual "for (parseInts [\"123\", \"abc\"])," [Just 123, Nothing] (parseInts ["123", "abc"]))
    ]

testHasInvalidOp = TestList [
    TestCase (assertBool "for (hasInvalidOp [\"sa\", \"sb\"])," (not (hasInvalidOp ["sa", "sb"]))),
    TestCase (assertBool "for (hasInvalidOp [\"sa\", \"IP\"])," (hasInvalidOp ["sa", "IP"]))
    ]

testHasInvalidInt = TestList [
    TestCase (assertBool "for (hasInvalidInt [123, 456])," (not (hasInvalidInt [Just 123, Just 456]))),
    TestCase (assertBool "for (hasInvalidInt [123, -1])," (not (hasInvalidInt [Just 123, Just (-1)]))),
    TestCase (assertBool "for (hasInvalidInt [123, Nothing])," (hasInvalidInt [Just 123, Nothing]))
    ]


