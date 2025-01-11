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
import My (myQuickSort)
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
isOperator "qsa" = True
isOperator "qsb" = True
isOperator "qsc" = True
isOperator "rdm" = True
isOperator "sfa" = True
isOperator "sfb" = True
isOperator "sfc" = True
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
    if hasInvalidInt i then
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

-- print the list with color : green if sorted, red otherwise number per number
myshowfinal :: [Int] -> [Int] -> String
myshowfinal [] _ = ""
myshowfinal [x] [] = "\ESC[31m" ++ show x ++ "\ESC[0m"
myshowfinal (x:xs) [] = "\ESC[31m" ++ show x ++ "\ESC[0m," ++ myshowfinal xs []
myshowfinal [x] [y] | x == y = "\ESC[32m" ++ show x ++ "\ESC[0m"
    | otherwise = "\ESC[31m" ++ show x ++ "\ESC[0m"
myshowfinal (x:xs) (y:ys) | x == y = "\ESC[32m" ++ show x ++ "\ESC[0m," ++ myshowfinal xs ys
    | otherwise = "\ESC[31m" ++ show x ++ "\ESC[0m," ++ myshowfinal xs ys

resultMessage :: [Int] -> [Int] -> String
resultMessage final_l_a final_l_b
    | isSorted final_l_a && null final_l_b = "\ESC[32mOK\ESC[0m: ([" ++ myshowfinal final_l_a (myQuickSort (<) final_l_a) ++ "],[" ++ myshowfinal final_l_b (myQuickSort (<) final_l_b) ++ "])"
    | otherwise = "\ESC[31mKO\ESC[0m: ([" ++ myshowfinal final_l_a (myQuickSort (<) final_l_a) ++ "],[" ++ myshowfinal final_l_b (myQuickSort (<) final_l_b) ++ "])"
