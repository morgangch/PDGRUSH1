{-
-- EPITECH PROJECT, 2025
-- $Rush01
-- File description:
-- $CommandChecker.hs
-}

module CommandChecker (isSorted, doOperation) where
import System.Environment()
import Data.List (sort)
import Utilities (swap, swapb, pa, pb, rotate, rr, rotaterev, rrr, qs, qsc)
import My()

isSorted :: [Int] -> Bool
isSorted xs = xs == sort xs

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
doOperation (l_a, l_b) _ = (l_a, l_b)
