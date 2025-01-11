{-
-- EPITECH PROJECT, 2025
-- $Rush01
-- File description:
-- $CommandChecker.hs
-}

module CommandChecker (isSorted, doOperation) where
import System.Environment()
import Data.List (sort)
import Utilities (swap, swapb, pa, pb, rotate, rr, rotaterev, rrr, qs, qsc, shuffleOne, shuffleTwo, generateSeed)
import My()

isSorted :: [Int] -> Bool
isSorted xs = xs == sort xs

interpreteRandomInstructions :: Int -> ([Int], [Int]) -> ([Int], [Int])
interpreteRandomInstructions x (y,ys)
    | x `mod` 18 == 0 = doOperation (y, ys) "sfc"
    | x `mod` 17 == 0 = doOperation (y, ys) "sfb"
    | x `mod` 16 == 0 = doOperation (y, ys) "sfa"
    | x `mod` 15 == 0 = doOperation (y, ys) "qsc"
    | x `mod` 14 == 0 = doOperation (y, ys) "qsb"
    | x `mod` 13 == 0 = doOperation (y, ys) "qsa"
    | x `mod` 12 == 0 = doOperation (y, ys) "rrr"
    | x `mod` 11 == 0 = doOperation (y, ys) "rrb"
    | x `mod` 10 == 0 = doOperation (y, ys) "rra"
    | x `mod` 9 == 0 = doOperation (y, ys) "rr"
    | x `mod` 8 == 0 = doOperation (y, ys) "rb"
    | x `mod` 7 == 0 = doOperation (y, ys) "ra"
    | x `mod` 6 == 0 = doOperation (y, ys) "pb"
    | x `mod` 5 == 0 = doOperation (y, ys) "pa"
    | x `mod` 4 == 0 = doOperation (y, ys) "sc"
    | x `mod` 3 == 0 = doOperation (y, ys) "sb"
    | x `mod` 2 == 0 = doOperation (y, ys) "sa"
    | otherwise = interpreteRandomInstructions (x `div` 18) (y, ys)

randomInstructions :: [Int] ->  [Int] -> ([Int], [Int])
randomInstructions l1 l2 = interpreteRandomInstructions (generateSeed l1 l2 * generateSeed l2 l1) (l1, l2)

doOperation :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperation (l_a, l_b) "sa" = (swap l_a, l_b)
doOperation (l_a, l_b) "sb" = (l_a, swap l_b)
doOperation (l_a, l_b) "sc" = swapb l_a l_b
doOperation (l_a, l_b) "pa" = pa l_a l_b
doOperation (l_a, l_b) "pb" = pb l_a l_b
doOperation (l_a, l_b) "ra" = (rotate l_a, l_b)
doOperation (l_a, l_b) "rb" = (l_a, rotate l_b)
doOperation (l_a, l_b) "rr" = rr l_a l_b
doOperation (l_a, l_b) "rra" = (rotaterev l_a, l_b)
doOperation (l_a, l_b) "rrb" = (l_a, rotaterev l_b)
doOperation (l_a, l_b) "rrr" = rrr l_a l_b
doOperation (l_a, l_b) "qsa" = (qs l_a, l_b)
doOperation (l_a, l_b) "qsb" = (l_a, qs l_b)
doOperation (l_a, l_b) "qsc" = qsc l_a l_b
doOperation (l_a, l_b) "rdm" = randomInstructions l_a l_b
doOperation (l_a, l_b) "sfa" = (shuffleOne l_a, l_b)
doOperation (l_a, l_b) "sfb" = (l_a, shuffleOne l_b)
doOperation (l_a, l_b) "sfc" = shuffleTwo l_a l_b
doOperation (l_a, l_b) _ = (l_a, l_b)
