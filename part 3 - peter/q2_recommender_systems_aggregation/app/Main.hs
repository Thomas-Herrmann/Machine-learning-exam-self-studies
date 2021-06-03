module Main where

import Recommend
import Data.Set as Set

main :: IO ()
main = do 
    putStrLn "pagerank:"
    writeFile "hetero.dot" $ show heteroGraph 
    print $ pagerank heteroGraph ([("USER", "Matt"), ("TIME", "Morning")], "SONG", 3)
    putStrLn "ne-aggregation->pagerank:"
    let agg = neAggregate heteroGraph aggPattern (Set.fromList ["USER", "TIME"], Set.singleton "SONG") ("{USER, TIME}", "{SONG}")
    writeFile "agg.dot" $ show agg
    print $ pagerank  agg ([("{USER, TIME}", Set.fromList [("TIME", "Morning"), ("USER", "Matt")])], "{SONG}", 3)
    putStrLn "bipartite->pagerank:"
    let bp = mkbipartite heteroGraph ([(Set.singleton "USER", "{USER}", 0.3), (Set.singleton "TIME", "{TIME}", 0.3), (Set.fromList ["USER", "TIME"], "{USER, TIME}", 0.4)], 0.5)
                                     ([(Set.singleton "COUNTRY","{COUNTRY}", 0.3), (Set.singleton "GENRE", "{GENRE}", 0.3), (Set.fromList ["COUNTRY", "GENRE"], "{COUNTRY, GENRE}", 0.4)], 0.5)
                                     ("SONG", "PLAY")
    writeFile "bp.dot" $ show bp
    print $ pagerank bp ([("{USER}", Set.fromList [("USER","Matt")]), ("{TIME}", Set.fromList [("TIME","Morning")])], "SONG", 3)