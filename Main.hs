module Main where
import System.Exit (exitWith, exitSuccess)

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

parseArgs :: [String] -> IO ()
parseArgs [] = putStrLn "No arguments"
parseArgs (x:xs)
    | isOperator x = putStrLn x >> parseArgs xs
    | otherwise = parseArgs xs

main :: IO ()
main = do
    exitSuccess