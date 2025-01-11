{-
-- EPITECH PROJECT, 2025
-- rush1
-- File description:
-- Main
-}

module Main where
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Environment (getArgs)
import Data.Char (isDigit)
import CommandChecker (isSorted, doOperation)
import Text.Read (readMaybe)

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
isOperator " " = False
isOperator _ = False

parseArgs :: String -> Maybe [String]
parseArgs [] = Just []
parseArgs str
    | length str >= 2 && isOperator (take 2 str) =
        case parseArgs (drop 2 str) of
            Just ops -> Just (take 2 str : ops)
            Nothing -> Just [take 2 str]
    | otherwise = case head str of
        ' ' -> parseArgs (tail str)
        '\n' -> parseArgs (tail str)
        _ -> Just ["IP"]

myReadMaybe :: String -> Int
myReadMaybe s = case readMaybe s of
    Just x -> x
    Nothing -> -1

parseInts :: [String] -> [Int]
parseInts [] = []
parseInts (x:xs)
    | all isDigit x = myReadMaybe x : parseInts xs
    | otherwise = (-1) : parseInts xs

hasInvalidOp :: [String] -> Bool
hasInvalidOp [] = False
hasInvalidOp ["IP"] = True
hasInvalidOp (_:xs) = hasInvalidOp xs

hasInvalidInt :: [Int] -> Bool
hasInvalidInt [] = False
hasInvalidInt (x:xs)
    | x == -1 = True
    | otherwise = hasInvalidInt xs

main :: IO ()
main = do
    args <- getArgs
    let i = parseInts args
    if null args || hasInvalidInt i then
        exitWith (ExitFailure 84)
    else do
        line <- getLine
        let s = parseArgs line
        processArgs i s

processArgs :: [Int] -> Maybe [String] -> IO ()
processArgs l_a (Just args)
    | null args = exitWith (ExitFailure 84)
    | hasInvalidOp args = exitFailure
    | otherwise =
        let (final_l_a, final_l_b) = foldl doOperation (l_a, []) args
        in putStrLn $ resultMessage final_l_a final_l_b
processArgs _ Nothing = exitWith (ExitFailure 84)

resultMessage :: [Int] -> [Int] -> String
resultMessage final_l_a final_l_b
    | isSorted final_l_a = "\ESC[32mOK\ESC[0m"
    | otherwise = "\ESC[31mKO\ESC[0m: (" ++ show final_l_a ++ "," ++ show final_l_b ++ ")"
