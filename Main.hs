{-
-- EPITECH PROJECT, 2025
-- rush1
-- File description:
-- Main
-}

module Main where
import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import CommandChecker (isSorted, doOperation)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, catMaybes)

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

parseInts :: [String] -> [Maybe Int]
parseInts [] = []
parseInts (x:xs) = readMaybe x : parseInts xs

hasInvalidOp :: [String] -> Bool
hasInvalidOp [] = False
hasInvalidOp ["IP"] = True
hasInvalidOp (_:xs) = hasInvalidOp xs

hasInvalidInt :: [Maybe Int] -> Bool
hasInvalidInt [] = False
hasInvalidInt (x:xs)
    | isNothing x = True
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
        processArgs (catMaybes i) s

processArgs :: [Int] -> Maybe [String] -> IO ()
processArgs l_a (Just args)
    | null args = putStrLn $ resultMessage l_a []
    | hasInvalidOp args = exitWith (ExitFailure 84)
    | otherwise =
        let (final_l_a, final_l_b) = foldl doOperation (l_a, []) args
        in putStrLn $ resultMessage final_l_a final_l_b
processArgs l_a Nothing = putStrLn $ resultMessage l_a []

resultMessage :: [Int] -> [Int] -> String
resultMessage final_l_a final_l_b
    | isSorted final_l_a && null final_l_b = "OK"
    | otherwise = "KO: (" ++ show final_l_a ++ "," ++ show final_l_b ++ ")"
