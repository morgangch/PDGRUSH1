{-
-- EPITECH PROJECT, 2025
-- $Rush01
-- File description:
-- $CommandChecker.hs
-}

import System.Environment (getArgs)
import Data.List (sort)

isSorted :: [Int] -> Bool
isSorted xs = xs == sort xs

doOperation :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperation (l_a, l_b) op = 
    let (new_l_a, new_l_b) = doOperationA (l_a, l_b) op
    in doOperationB (new_l_a, new_l_b) op

doOperationA :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperationA (l_a, l_b) op = case op of
    "sa" -> (sa l_a, l_b)
    "ra" -> (ra l_a, l_b)
    "rra" -> (rra l_a, l_b)
    _    -> (l_a, l_b)

doOperationB :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperationB (l_a, l_b) op = case op of
    "sb" -> (l_a, sb l_b)
    "rb" -> (l_a, rb l_b)
    "rrb" -> (l_a, rrb l_b)
    "ss" -> ss (l_a, l_b)
    "pa" -> pa (l_a, l_b)
    "pb" -> pb (l_a, l_b)
    "rr" -> rr (l_a, l_b)
    "rrr" -> rrr (l_a, l_b)
    _    -> (l_a, l_b)
