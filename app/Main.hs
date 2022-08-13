module Main where

import System.Environment
import System.Exit
import Data.Functor

import Lib

printStats :: [Int] -> IO ()
printStats x = do
    putStrLn . showAve    10 $ x
    putStrLn . showStdDev 10 $ x

main :: IO ()
main = getArgs >>= parse >>= printStats

parse :: [[Char]] -> IO [Int]
parse ["-h"] = usage    >> exitSuccess
parse ["-v"] = version  >> exitSuccess
parse [] = getContents <&> fmap (read :: String -> Int) . lines


usage   = putStrLn "Usage: ./stats [-hv]"
version = putStrLn "Quick Stats Widget v0.1.0.0"
