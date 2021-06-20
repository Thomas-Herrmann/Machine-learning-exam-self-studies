module Main where

import Recommend
import Data.Set as Set
import Data.Map as Map

main :: IO ()
main = do 
    putStrLn "pagerank:"
    writeFile "hetero.dot" $ show heteroGraph
    let prhg = pagerank heteroGraph ([("USER", "Matt"), ("TIME", "Morning")], "SONG", 3) 
    print prhg
    writeFile "heteropr.dot" $ show (addScores heteroGraph $ Map.fromList prhg)
    putStrLn "ne-aggregation->pagerank:"
    let agg = neAggregate heteroGraph aggPattern (Set.fromList ["USER", "TIME"], Set.singleton "SONG") ("{USER, TIME}", "{SONG}")
    writeFile "agg.dot" $ show agg
    let pragg = pagerank agg ([("{USER, TIME}", Set.fromList [("TIME", "Morning"), ("USER", "Matt")])], "{SONG}", 3)
    print pragg
    writeFile "aggpr.dot" $ show (addScores agg $ Map.fromList pragg)
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
    let prbpcf = pagerank bpcf ([("{USER}", Set.fromList [("USER","Matt")])], "SONG", 3)
    print prbpcf
    writeFile "bpcfpr.dot" $ show (addScores bpcf $ Map.fromList prbpcf)