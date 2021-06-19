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
    let bp = mkbipartite heteroGraph ([(Set.singleton "USER", "{USER}", 0.5), (Set.fromList ["USER", "TIME"], "{USER, TIME}", 0.5)], 0.5)
                                     ([(Set.singleton "COUNTRY","{COUNTRY}", 0.5), (Set.singleton "GENRE", "{GENRE}", 0.5)], 0.5)
                                     ("SONG", "PLAY")
    writeFile "bp.dot" $ show bp
    let bpca = mkbipartite heteroGraph ([(Set.fromList ["USER", "TIME"], "{USER, TIME}", 1.0)], 1.0)
                                     ([], 0.0)
                                     ("SONG", "PLAY")
    writeFile "bpca.dot" $ show bpca
    let bpcf = mkbipartite heteroGraph ([(Set.fromList ["USER"], "{USER}", 1.0)], 1.0)
                                     ([], 0.0)
                                     ("SONG", "PLAY")
    writeFile "bpcf.dot" $ show bpcf
    print $ pagerank bp ([("{USER}", Set.fromList [("USER","Matt")]), ("{TIME}", Set.fromList [("TIME","Morning")])], "SONG", 3)