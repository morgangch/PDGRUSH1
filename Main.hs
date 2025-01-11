{-
-- EPITECH PROJECT, 2025
-- rush1
-- File description:
-- Main
-}

module Main where
import System.Exit()

isOperator :: String -> Bool
isOperator "sa" = True
isOperator "sb" = True
isOperator "sc" = True
isOperator "pa" = True
isOperator "pb" = True
isOperator "ra" = True
isOperator "rb" = True
isOperator "rr" = True
isOperator "rra" = True
isOperator "rrb" = True
isOperator "rrr" = True
isOperator _ = False

parseArgs :: String -> IO ()
parseArgs [] = putStrLn "No arguments"
parseArgs [_] = putStrLn "Incomplete argument"
parseArgs (x:y:xs)
    | isOperator [x,y] = putStrLn [x,y] >> parseArgs xs
    | otherwise = parseArgs xs

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

main :: IO ()
main = do
    s <- getLine
    parseArgs s