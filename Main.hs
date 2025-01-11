{-
-- EPITECH PROJECT, 2025
-- rush1
-- File description:
-- Main
-}

module Main where
import System.Exit(exitFailure)
import System.Environment (getArgs)
import Data.Char (isDigit)
import CommandChecker (isSorted, doOperation)

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

parseArgs :: String -> Maybe [String]
parseArgs [] = Nothing
parseArgs [_] = Nothing
parseArgs (x:y:xs)
    | isOperator [x,y] = case parseArgs xs of
                            Just ops -> Just ([x,y] : ops)
                            Nothing -> Just [[x,y]]
    | otherwise = parseArgs xs

parseInts :: String -> [Int]
parseInts [] = []
parseInts (x:xs)
    | isDigit x = read [x] : parseInts xs
    | otherwise = parseInts xs

main :: IO ()
main = do
    args <- getArgs
    let i = parseInts (concat args)
    if null args then
        exitFailure
    else do
        line <- getLine
        let s = parseArgs line
        processArgs i s

processArgs :: [Int] -> Maybe [String] -> IO ()
processArgs l_a (Just args)
    | null args = exitFailure
    | otherwise = do
        let (final_l_a, final_l_b) = foldl doOperation (l_a, []) args
        putStrLn $ resultMessage final_l_a final_l_b

resultMessage :: [Int] -> [Int] -> String
resultMessage final_l_a final_l_b
    | isSorted final_l_a && null final_l_b = "OK"
    | otherwise = "KO: (" ++ show final_l_a ++ "," ++ show final_l_b ++ ")"
