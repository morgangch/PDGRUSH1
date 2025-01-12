    module TestBonuses where
    import Test.HUnit
    import Utilities (qs, qsc, generateSeed, shuffleOne, shuffleTwo)
    import CommandChecker (doOperation)
    import Main (myshowfinal, resultMessage)
    
    testQs :: Test
    testQs = TestList [
        "qs empty list" ~: qs [] ~?= [],
        "qs single element list" ~: qs [1] ~?= [1],
        "qs multiple elements list" ~: qs [3, 1, 2] ~?= [1, 2, 3]
        ]

    testQsc :: Test
    testQsc = TestList [
        "qsc empty lists" ~: qsc [] [] ~?= ([], []),
        "qsc non-empty lists" ~: qsc [3, 1, 2] [6, 5, 4] ~?= ([1, 2, 3], [4, 5, 6])
        ]

    testGenerateSeed :: Test
    testGenerateSeed = TestList [
        "generateSeed empty lists" ~: generateSeed [] [] ~?= 0,
        "generateSeed non-empty lists" ~: generateSeed [1, 2, 3] [4, 5, 6] ~?= (((sum [1, 2, 3] + sum [4, 5, 6]) 
        * sum [1, 2, 3] + sum [4, 5, 6] * 5 * 7) `div` 2)
        ]

    testShuffleOne :: Test
    testShuffleOne = TestList [
        "shuffleOne empty list" ~: shuffleOne [] ~?= [],
        "shuffleOne single element list" ~: shuffleOne [1] ~?= [1],
        "shuffleOne multiple elements list" ~: length (shuffleOne [1, 2, 3, 4]) ~?= 4
        ]

    testShuffleTwo :: Test
    testShuffleTwo = TestList [
        "shuffleTwo empty lists" ~: shuffleTwo [] [] ~?= ([], []),
        "shuffleTwo non-empty lists" ~: let (l1, l2) = shuffleTwo [1, 2] [3, 4] in (length l1, length l2) ~?= (2, 2)
        ]

    testDoOperation = TestList [
        TestCase (assertEqual "qsa operation" ([3, 199, 334], [4322, 432]) (doOperation ([199, 334, 3], [4322, 432]) "qsa")),
        TestCase (assertEqual "qsb operation" ([1, 2, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "qsb")),
        TestCase (assertEqual "qsc operation" ([1, 2, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "qsc")),
        
        TestCase (assertEqual "sfa operation" ([1, 2, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "sfa")),
        TestCase (assertEqual "sfb operation" ([1, 2, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "sfb")),
        TestCase (assertEqual "sfc operation" ([3,1,2],[4,5]) (doOperation ([1, 2, 3], [4, 5]) "sfc")),
        
        TestCase (assertEqual "Unknown operation" ([1, 2, 3], [4, 5]) (doOperation ([1, 2, 3], [4, 5]) "unknown"))
        ]
    testMyshowfinal = TestList [
        TestCase (assertEqual "for (myshowfinal [1, 2, 3] [1, 2, 3])," "\ESC[32m1\ESC[0m,\ESC[32m2\ESC[0m,\ESC[32m3\ESC[0m" (myshowfinal [1, 2, 3] [1, 2, 3])),
        TestCase (assertEqual "for (myshowfinal [1, 2, 3] [3, 2, 1])," "\ESC[31m1\ESC[0m,\ESC[32m2\ESC[0m,\ESC[31m3\ESC[0m" (myshowfinal [1, 2, 3] [3, 2, 1])),
        TestCase (assertEqual "for (myshowfinal [1] [1])," "\ESC[32m1\ESC[0m" (myshowfinal [1] [1])),
        TestCase (assertEqual "for (myshowfinal [1] [2])," "\ESC[31m1\ESC[0m" (myshowfinal [1] [2])),
        TestCase (assertEqual "for (myshowfinal [] [])," "" (myshowfinal [] []))
        ]

    testResultMessage = TestList [
        TestCase (assertEqual "for (resultMessage [1, 2, 3] [])," "\ESC[32mOK\ESC[0m: ([\ESC[32m1\ESC[0m,\ESC[32m2\ESC[0m,\ESC[32m3\ESC[0m],[])" (resultMessage [1, 2, 3] [])),
        TestCase (assertEqual "for (resultMessage [3, 2, 1] [])," "\ESC[31mKO\ESC[0m: ([\ESC[31m3\ESC[0m,\ESC[32m2\ESC[0m,\ESC[31m1\ESC[0m],[])" (resultMessage [3, 2, 1] [])),
        TestCase (assertEqual "for (resultMessage [1, 2, 3] [4, 5, 6])," "\ESC[31mKO\ESC[0m: ([\ESC[32m1\ESC[0m,\ESC[32m2\ESC[0m,\ESC[32m3\ESC[0m],[\ESC[32m4\ESC[0m,\ESC[32m5\ESC[0m,\ESC[32m6\ESC[0m])" (resultMessage [1, 2, 3] [4, 5, 6])),
        TestCase (assertEqual "for (resultMessage [] [])," "\ESC[32mOK\ESC[0m: ([],[])" (resultMessage [] []))
        ]
    main :: IO ()
    main = do
        runTestTT $ TestList [
            testQs,
            testQsc,
            testGenerateSeed,
            testShuffleOne,
            testShuffleTwo,
            testDoOperation,
            testMyshowfinal,
            testResultMessage
            ]
        return ()

