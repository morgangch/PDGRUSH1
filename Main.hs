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

main :: IO ()
main = do
    s <- getLine
    parseArgs s