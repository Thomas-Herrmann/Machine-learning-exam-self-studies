module Recommend
( heteroGraph
, aggPattern
, pagerank
, adjMatrix
, neAggregate
, gmerge
, mkbipartite
) where


import Data.Map as Map
import Data.Bimap as Bimap
import Data.Set as Set hiding((!)) 
import Data.List as List hiding(transpose)
import Data.Array as Array
import Data.Matrix as Matrix
import Data.Maybe(fromJust)
import Data.Bifunctor(bimap, first)
import Data.Ord(Ordering(..))
import Data.Tuple(swap)


data Graph a n = Graph (Array Int [Int]) (Array Int [Int]) (Map (a, a) n) (Bimap a Int)

instance (Ord a, Show a, Show n, Fractional n) => Show (Graph a n) where
    
    show g@(Graph _ _ wmap vmap) = "digraph G {\n" 
                                ++ concatMap (\((u, v), w) -> show (vmap Bimap.! u) ++ " -> " ++ show (vmap Bimap.! v) ++ "[label=\"" ++ show w ++ "\"];\n") wes 
                                ++ intercalate "\n" (Prelude.map (\u -> show (vmap Bimap.! u) ++ " [label=\"" ++ (escape . show) u ++ "\"];") $ vertices g)
                                ++ "\n}"
        where
            wes = Prelude.map (\e -> (e, wmap Map.! e)) $ edges g

            escape []         = []
            escape ('"' : s') = '\\' : '"' : escape s'
            escape (c : s')   = c : escape s' 


type Query t a = ([(t, a)], t, Int)


adjMatrix :: (Ord a, Fractional n) => Graph a n -> Matrix n
adjMatrix g@(Graph _ _ wmap vmap) = matrix n n cellVal
    where
        n = length $ vertices g

        toEdge = bimap (vmap !>) (vmap !>)

        cellVal e = Prelude.foldr ((+) . (wmap Map.!)) 0 $ Prelude.filter (`Map.member` wmap) [toEdge e, (swap . toEdge) e]


transMatrix :: (Ord a, Fractional n) => Graph a n -> Matrix n
transMatrix g@(Graph _ _ _ vmap) = matrix n n cellVal
    where
        n = length $ vertices g
        m = adjMatrix g

        cellVal e@(i, j) | List.null (outAdj g (vmap !> i)) = 0
                         | otherwise                        = (m Matrix.! e) / sum (Prelude.map (\v -> m Matrix.! (i, vmap Bimap.! v)) (outAdj g (vmap !> i)))


pagerank :: (Ord a, Eq t, Ord t, Ord n, Fractional n) => Graph (t, a) n -> Query t a -> [((t, a), n)]
pagerank g@(Graph _ _ _ vmap) (n, 𝜏, k) = Prelude.take k $
                                          sortBy (\(_, r1) (_, r2) -> r2 `compare` r1) $
                                          Prelude.filter ((𝜏 ==) . fst . fst) $ 
                                          List.zipWith (curry (first (vmap !>))) [1, 2..] $ 
                                          Matrix.toList $ aux (matrix (length vs) 1 $ const (1.0 / fromIntegral (length vs)))
    where
        vs        = vertices g
        c         = 0.85
        querySize = fromIntegral . length $ n
        q         = Matrix.fromList (length vs) 1 $ Prelude.map ((/ querySize) . fromIntegral . fromEnum . (`elem` n)) vs
        p         = transMatrix g

        aux r | maxChange < threshold = r'
              | otherwise             = aux r' 
            where
                r'        = scaleMatrix c (transpose p) * r + scaleMatrix (1 - c) q
                maxChange = maximum $ Matrix.elementwise (\r1 r2 -> abs (r1 - r2)) r' r
                threshold = 0.00001

-- Aggregation

neAggregate :: (Ord a, Eq t, Ord t, Ord n, Fractional n) => Graph (t, a) n -> Graph t n -> (Set t, Set t) -> (t, t) -> Graph (t, Set (t, a)) n
neAggregate g@(Graph _ _ wmap vmap) a (p1, p2) (𝜏1, 𝜏2) = mkgraph $ concat [aggEdges inP1 inP2 | inP1 <- insP1, inP2 <- insP2]
    where
        vs    = vertices g
        es    = edges g
        esa   = edges a
        insP1 = Prelude.filter (isInstance p1) $ Prelude.map Set.fromList $ subsequencesOfSize (length p1) vs
        insP2 = Prelude.filter (isInstance p2) $ Prelude.map Set.fromList $ subsequencesOfSize (length p2) vs

        sizedSG            = Prelude.map (mkgraph . Prelude.map (\e -> (e, wmap Map.! e))) $ subsequencesOfSize (length esa) es
        matchingSG         = Prelude.filter ((Prelude.length (vertices a) ==) . Prelude.length . vertices) $ Prelude.filter (hasEdges esa . edges) sizedSG
        aggEdges inP1 inP2 = Prelude.filter ((0.0 /= ) . snd) [(((𝜏1, inP1), (𝜏2, inP2)), fromIntegral $ length includingSG), (((𝜏2, inP2), (𝜏1, inP1)), fromIntegral $ length includingSG)]
            where
                includingSG = Prelude.filter (((inP1 `Set.union` inP2) `isSubsetOf`) . Set.fromList . vertices) matchingSG

        hasEdges esa' es' = fst $ Prelude.foldr satisfies (True, Set.fromList es') $ Set.fromList esa'
            where
                satisfies ea (b, es'') =
                    case find ((ea ==) . bimap fst fst) es'' of
                        Nothing -> (False, Set.empty)
                        Just e  -> (b, Set.delete e es'')

        isInstance p i = fst $ Set.foldr satisfies (True, i) p
            where
                satisfies 𝜏 (b, i') =
                    case find ((𝜏 ==) . fst) i' of
                        Nothing -> (False, Set.empty)
                        Just u  -> (b, Set.delete u i')


gmerge :: (Ord a, Eq t, Ord t, Ord n, Fractional n) => [Graph (t, a) n] -> Graph (t, a) n
gmerge gs = mkgraph $ Prelude.foldr mergeEdges [] gs
    where
        mergeEdges g@(Graph _ _ wmap _) wes = 
            let (es, rem) = Prelude.foldr (addEdge wmap) ([], Set.fromList $ edges g) wes
            in  es ++ Prelude.map (\e -> (e, wmap Map.! e)) (Set.toList rem)

        addEdge wmap (e, w) (es, rem) | e `Set.member` rem = ((e, w + wmap Map.! e) : es, Set.delete e rem)
                                      | otherwise          = ((e, w) : es, rem)


mkbipartite :: (Ord a, Eq t, Ord t, Ord n, Fractional n) => Graph (t, a) n -> ([(Set t, t, n)], n) -> ([(Set t, t, n)], n) -> (t, t) -> Graph (t, Set (t, a)) n
mkbipartite g (fc, wfc) (ft, wft) (ntI, ntT) = gmerge $ gcs ++ gts
    where
        gcs = [mapWeights ( * (wfc * wcf)) $ neAggregate g (mkAC cf) (cf, Set.singleton ntI) (nct, ntI) | (cf, nct, wcf) <- fc]
        gts = [mapWeights ( * (wft * wtf)) $ neAggregate g (mkAT tf) (tf, Set.singleton ntI) (ntt, ntI) | (tf, ntt, wtf) <- ft]

        mkAC cf = mkgraph $ ((ntT, ntI), 1) : [((ntT, nc), 1) | nc <- Set.toList cf]
        mkAT tf = mkgraph [((ntI, nt), 1) | nt <- Set.toList tf]

        mapWeights f (Graph oarr iarr wmap vmap) = Graph oarr iarr (Map.map f wmap) vmap

-- Graph utility

outAdj :: (Ord a, Fractional n) => Graph a n -> a -> [a]
outAdj (Graph oarr _ _ vmap) = Prelude.map (vmap !>) . (Array.!) oarr . (Bimap.!) vmap


inAdj :: (Ord a, Fractional n) => Graph a n -> a -> [a]
inAdj (Graph _ iarr _ vmap) = Prelude.map (vmap !>) . (Array.!) iarr . (Bimap.!) vmap


edges :: (Ord a, Fractional n) => Graph a n -> [(a, a)]
edges (Graph arr _ _ vmap) = Prelude.map (bimap (vmap !>) (vmap !>)) $ Prelude.foldr (\(v, vs) es -> Prelude.foldr (\u es' -> (v, u) : es') es vs) [] $ Array.assocs arr


vertices :: (Ord a, Fractional n) => Graph a n -> [a]
vertices (Graph arr _ _ vmap) = Prelude.map (vmap !>) $ Array.indices arr


mkgraph :: (Ord a, Fractional n) => [((a, a), n)] -> Graph a n
mkgraph wes = Graph (mkarray mappedes) (mkarray $ Prelude.map swap mappedes) wmap vmap
    where
        (es, ws) = Prelude.unzip wes
        mappedes = Prelude.map (bimap (vmap Bimap.!) (vmap Bimap.!)) es
        vs       = let (from, to) = Prelude.unzip es in Prelude.foldr (List.union . (: [])) [] $ from ++ to
        vmap     = Bimap.fromList $ Prelude.zip vs [1, 2..]
        wmap     = Map.fromList wes
        mkarray  = Array.accumArray (flip (:)) [] (1, length vs)


-- Extra utils

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs | n <= l = subsequencesBySize xs !! (l - n)
 where
    l = length xs

    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) = zipWith (++) ([] : next) $ Prelude.map (Prelude.map (x :)) next ++ [[]]  
        where
            next = subsequencesBySize xs

subsequencesOfSize _ _ = []


-- Experiments

heteroGraph :: Graph (String, String) Float
heteroGraph = mkgraph $ Prelude.zip [ (("PLAY", "Play#1"), ("TIME", "Night")), (("PLAY", "Play#2"), ("TIME", "Night"))
                                    , (("PLAY", "Play#3"), ("TIME", "Morning")), (("PLAY", "Play#4"), ("TIME", "Morning"))
                                    , (("PLAY", "Play#1"), ("USER", "Matt")), (("PLAY", "Play#2"), ("USER", "Matt")), (("PLAY", "Play#3"), ("USER", "Matt")), (("PLAY", "Play#4"), ("USER", "Matt"))
                                    , (("PLAY", "Play#1"), ("SONG", "Song#1")), (("PLAY", "Play#2"), ("SONG", "Song#1"))
                                    , (("PLAY", "Play#3"), ("SONG", "Song#2"))
                                    , (("PLAY", "Play#4"), ("SONG", "Song#3"))
                                    , (("SONG", "Song#1"), ("GENRE", "Jazz")), (("SONG", "Song#2"), ("GENRE", "Jazz"))
                                    , (("SONG", "Song#3"), ("GENRE", "Pop"))
                                    , (("SONG", "Song#1"), ("COUNTRY", "American"))
                                    , (("SONG", "Song#2"), ("COUNTRY", "Korean")), (("SONG", "Song#3"), ("COUNTRY", "Korean"))
                                    ] [1, 1..]


aggPattern :: Graph String Float
aggPattern = mkgraph $ Prelude.zip [ ("PLAY", "USER"), ("PLAY", "TIME"), ("PLAY", "SONG")
                                   ] [1, 1..]


