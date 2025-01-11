{-
-- EPITECH PROJECT, 2025
-- $Rush01
-- File description:
-- $Utilities.hs
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Utilities (swap, swapb, pa, pb, rotate, rr, rotaterev, rrr, qs, qsc, shuffleOne, shuffleTwo, generateSeed) where
import My (myQuickSort)

swap :: [a] -> [a]
swap [] = []
swap [x] = [x]
swap (x:y:xs) = y : x : xs

swapb :: [a] -> [a] -> ([a], [a])
swapb l1 l2 = (swap l1, swap l2)

pa :: [a] -> [a] -> ([a], [a])
pa l1 [] = (l1, [])
pa l1 (x:xs) = (x : l1, xs)

pb :: [a] -> [a] -> ([a], [a])
pb [] l2 = ([], l2)
pb (x:xs) l2 = (xs, x : l2)

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

rr :: [a] -> [a] -> ([a], [a])
rr l1 l2 = (rotate l1, rotate l2)

rotaterev :: [a] -> [a]
rotaterev [] = []
rotaterev list = last list : init list

rrr :: [a] -> [a] -> ([a], [a])
rrr l1 l2 = (rotaterev l1, rotaterev l2)

qs :: [Int] -> [Int]
qs = myQuickSort (<)

qsc :: [Int] -> [Int] -> ([Int], [Int])
qsc l1 l2 = (qs l1, qs l2)

generateSeed :: [Int] -> [Int] -> Int
generateSeed la lb = 
    (fromIntegral (sum la + sum lb) * sum la + sum lb * 5 * 7) `div` 2

shuffleList :: [Int] -> [Int]
shuffleList [] = []
shuffleList xs =
    let seed = generateSeed xs []
        idx = seed `mod` length xs
        (left, a:right) = splitAt idx xs
    in a : shuffleList (left ++ right)

shuffleOne :: [Int] -> [Int]
shuffleOne l = shuffleList l

shuffleTwo :: [Int] -> [Int] -> ([Int], [Int])
shuffleTwo l1 l2 =
    let combined = shuffleList (l1 ++ l2)
        (newL1, newL2) = splitAt (length l1) combined
    in (newL1, newL2)
