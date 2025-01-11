{-
-- EPITECH PROJECT, 2025
-- $Rush01
-- File description:
-- $CommandChecker.hs
-}

module CommandChecker (isSorted, doOperation) where
import System.Environment (getArgs)
import Data.List (sort)
import Utilities (swap, swapb, pa, pb, rotate, rr, rotaterev, rrr)

isSorted :: [Int] -> Bool
isSorted xs = xs == sort xs

doOperation :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperation (l_a, l_b) op = 
    let (new_l_a, new_l_b) = doOperationA (l_a, l_b) op
        (final_l_a, final_l_b) = doOperationB (new_l_a, new_l_b) op
    in doOperationC (final_l_a, final_l_b) op

doOperationA :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperationA (l_a, l_b) op = case op of
    "sa" -> (swap l_a, l_b)
    "ra" -> (rotate l_a, l_b)
    "rra" -> (rotaterev l_a, l_b)
    _    -> (l_a, l_b)

doOperationB :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperationB (l_a, l_b) op = case op of
    "sb" -> (l_a, swap l_b)
    "rb" -> (l_a, rotate l_b)
    "rrb" -> (l_a, rotaterev l_b)
    _    -> (l_a, l_b)

doOperationC :: ([Int], [Int]) -> String -> ([Int], [Int])
doOperationC (l_a, l_b) op = case op of
    "sc" -> swapb (l_a, l_b)
    "pa" -> pa (l_a, l_b)
    "pb" -> pb (l_a, l_b)
    "rr" -> rr (l_a, l_b)
    "rrr" -> rrr (l_a, l_b)
    _    -> (l_a, l_b)
