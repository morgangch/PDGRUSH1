{-
-- EPITECH PROJECT, 2025
-- $Rush01
-- File description:
-- $Utilities.hs
-}

module Utilities (swap, swapb, pa, pb, rotate, rr, rotaterev, rrr) where

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
