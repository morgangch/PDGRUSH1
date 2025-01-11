{-
-- EPITECH PROJECT, 2025
-- rush1
-- File description:
-- Main
-}

module Main where
import System.Exit (exitWith, ExitCode(..))
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

parseArgs :: [String] -> Maybe [String]
parseArgs [] = Just []
parseArgs (x:xs)
    | isOperator x = case parseArgs xs of
        Just l -> Just (x : l)
        Nothing -> 
            if null xs then Just [x]
            else Just ["IP"]
    | otherwise = Just ["IP"]

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
        let s = parseArgs (words line)
        processArgs i s

processArgs :: [Int] -> Maybe [String] -> IO ()
processArgs l_a (Just args)
    | null args = exitWith (ExitFailure 84)
    | hasInvalidOp args = exitWith (ExitFailure 84)
    | otherwise =
        let (final_l_a, final_l_b) = foldl doOperation (l_a, []) args
        in putStrLn $ resultMessage final_l_a final_l_b
processArgs _ Nothing = exitWith (ExitFailure 84)

resultMessage :: [Int] -> [Int] -> String
resultMessage final_l_a final_l_b
    | isSorted final_l_a = "OK"
    | otherwise = "KO: (" ++ show final_l_a ++ "," ++ show final_l_b ++ ")"
