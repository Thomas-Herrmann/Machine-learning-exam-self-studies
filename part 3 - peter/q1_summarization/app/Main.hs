module Main where

import Summary
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    case args of
        ["exam"]         -> do
            putStrLn "non-clustered summary (Tullimonstrum WIKI):\n"
            doc <- readFile "tully_monster.txt" 
            print $ summary transitionProba (Just 5) doc
            putStrLn "\nclustered summary (Tullimonstrum WIKI):\n"
            let vs       = segmentSentences doc
            let clusters = kmeans vs $ ceiling (sqrt (fromIntegral (length vs))) 
            print $ summary (clusteredTransitionProba clusters) (Just 5) doc

        [path, "tp"]     -> readFile path >>= print . summary transitionProba Nothing
        [path, "tp", k]  -> readFile path >>= print . summary transitionProba (Just ((read :: String -> Int) k))

        [path, "ctp"]    -> do
            doc <- readFile "tully_monster.txt"
            let vs       = segmentSentences doc
            let clusters = kmeans vs $ ceiling (sqrt (fromIntegral (length vs)))   
            print $ summary (clusteredTransitionProba clusters) Nothing doc
        [path, "ctp", k] -> do
            doc <- readFile "tully_monster.txt"
            let vs       = segmentSentences doc
            let clusters = kmeans vs $ ceiling (sqrt (fromIntegral (length vs)))  
            print $ summary (clusteredTransitionProba clusters) (Just ((read :: String -> Int) k)) doc
        
        _                -> putStrLn "a single file path, transition probability type and k must be specified."
