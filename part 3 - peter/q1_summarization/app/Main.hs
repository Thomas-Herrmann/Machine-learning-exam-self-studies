module Main where

import Summary
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    case args of
        [path, "tp"]     -> readFile path >>= print . summary transitionProba Nothing
        [path, "tp", k]  -> readFile path >>= print . summary transitionProba (Just ((read :: String -> Int) k))
        [path, "ctp"]    -> readFile path >>= print . summary clusteredTransitionProba Nothing
        [path, "ctp", k] -> readFile path >>= print . summary clusteredTransitionProba (Just ((read :: String -> Int) k))
        _                -> putStrLn "a single file path, transition probability type and k must be specified."
