module Main where

import SimRank

main :: IO ()
main = do 
    putStrLn "saved webG^2"
    writeFile "g.dot" $ show webGraph
    writeFile "gsquared.dot" $ show (squareG webGraph)
    putStrLn "homogenous simrank(webG):"
    print $ homoSimRank 0.8 webGraph 10
    putStrLn "bipartite simrank(personItemG):"
    print $ bipartiteSimRank 0.8 personItemGraph 10
    writeFile "bp.dot" $ show personItemGraph 