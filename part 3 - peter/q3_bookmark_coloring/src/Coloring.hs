module Coloring
    ( bca
    , fbca
    , lfbca
    , exampleGraph
    , fromPQOrdered
    , Graphv(..)
    , visualExampleGraph
    , pqToMap
    , addScores
    , convertGtoVG
    , lfbcaGraph     
    ) where

import Data.Graph as Graph
import Data.Map as Map
import Data.Set as Set hiding((!)) 
import Data.List as List
import qualified Data.Array as Array
import Data.PSQueue as PQ
import Data.Maybe as Maybe
import qualified Data.Bimap as Bimap
import Data.Bifunctor(bimap)
import Data.Tuple(swap)


data SNNode = Location
            | User
            deriving Eq

type VertexMap = Map Vertex SNNode

type WeightMap = Map Edge Float

type PosMap = Map Vertex (Float, Float)

type SNGraph = (Graph, VertexMap, WeightMap, PosMap)


fromPQOrdered :: PSQ Vertex Float -> [(Vertex, Float)]
fromPQOrdered =  Prelude.map (\bind -> (key bind, prio bind)) . PQ.toAscList 


exampleGraph :: SNGraph
exampleGraph = ( buildG (0, 6) $ symmetrical [(0, 1), (2, 1), (2, 4), (3, 4), (1, 4), (4, 5), (0, 6)]
               , Map.fromList [(0, Location), (1, User), (2, Location), (3, Location), (4, User), (5, User), (6, User)]
               , Map.fromList $ symmetricalWeights [((0, 1), 7.0), ((2, 1), 3.0), ((2, 4), 1.0), ((3, 4), 1.0), ((1, 4), -1.0), ((4, 5), -1.0), ((0, 6), 2.0)]
               , Map.fromList [(0, (10.0, 1.0)), (2, (3.2, 2.3)), (3, (5.3, 4.4))]
               )


symmetrical :: [(a, a)] -> [(a, a)]
symmetrical = Prelude.foldr (\(one, two) l -> (one, two) : (two, one) : l) [] 


symmetricalWeights :: Num n => [((a, a), n)] -> [((a, a), n)]
symmetricalWeights = Prelude.foldr (\((one, two), w) l -> ((one, two), w) : ((two, one), w) : l) [] 


listUpdate :: (Eq t, Num t) => (a -> a) -> t -> [a] -> [a]
listUpdate f i l = aux l 0
    where
        aux [] _ = error "invalid list size"
        aux (e : es) j | j == i    = f e : es
                       | otherwise = e : aux es (j + 1)


bca :: SNGraph -> Vertex -> Float -> Float -> [Float]
bca (g, vm, wm, _) u α ε = aux initπ initb
    where
        vs = vertices g
        us = Prelude.filter ((User ==) . (vm !)) vs

        isUser v = (vm ! v) == User

        isFriend e@(v1, v2) | e `Map.member` wm = isUser v1 && isUser v2
                            | otherwise         = False
        
        friends v = Prelude.filter (\v' -> isFriend (v, v')) $ (Array.!) g v 

        initb = listUpdate (const 1) u $ Prelude.take (length vs) [0..]
        initπ = Prelude.take (length vs) [0..]
        
        d = Prelude.map (fromIntegral . length . friends) vs    

        aux π b =
            if b == b'
                then π'
                else aux π' b'
            where
                (π', b') = Prelude.foldr bmColor (π, b) us

        bmColor v state@(π, b) | (b !! v) < ε = state
                               | otherwise    = (π', b')
            where
                π' = listUpdate ((1 - α) * (b !! v) +) v π
                b' = zipWith (curry modb) [0, 1..] b
                modb (v', bj) | isFriend (v, v') && (wm ! (v, v')) >= 0 = bj + α * (b !! v) * (wm ! (v, v'))
                              | isFriend (v, v')                        = bj + α * (b !! v) / (d !! v)
                              | v' == v                                 = 0
                              | otherwise                               = bj


fbca :: SNGraph -> Vertex -> Float -> Int -> PSQ Vertex Float
fbca sng@(g, vm, wm, pm) u d n = Prelude.foldr makePQ PQ.empty ld
    where
        vs = vertices g
        ls = Prelude.filter ((Location ==) . (vm !)) vs
        us = Prelude.filter ((User ==) . (vm !)) vs
        
        isUser v = (vm ! v) == User

        isFriend e@(v1, v2) | e `Map.member` wm = isUser v1 && isUser v2
                            | otherwise         = False
        
        visited v = Prelude.filter (\v' -> not $ isFriend (v, v')) $ (Array.!) g v
        
        π = bca sng u 0.85 0.001
        
        lu = visited u
        ld = (List.\\) ls lu
        
        sInit = Map.fromList $ Prelude.zip ld [0..]
        s     = Prelude.foldr updateS sInit $ (List.\\) us [u]

        updateS :: Vertex -> Map Vertex Float -> Map Vertex Float
        updateS v s' = Prelude.foldr (\l -> Map.adjust (\sl -> sl + (π !! v) * (wm ! (v, l))) l) s' $ (List.\\) (visited v) lu 

        makePQ :: Vertex -> PSQ Vertex Float -> PSQ Vertex Float
        makePQ l r | geodist l lu > d       = r 
                   | PQ.size r < n          = PQ.insert l (s ! l) r
                   | (s ! l) <= minPrioPQ r = r
                   | otherwise              = PQ.insert l (s ! l) $ PQ.deleteMin r 

        geodist _ [] = 0
        geodist l ls = minimum $ Prelude.map (\l' -> let (x', y') = pm ! l' in sqrt $ (x - x') ** 2 + (y - y') ** 2) ls
            where
                (x, y) = pm ! l


minPrioPQ :: (Ord k, Ord p) => PSQ k p -> p
minPrioPQ = prio . fromJust . PQ.findMin 


cosineSim :: [Float] -> [Float] -> Float
cosineSim a b = (a `dot` b) / (mag a * mag b)
    where
        v1 `dot` v2 = Prelude.foldr (\(f1, f2) -> ((f1 * f2) +)) 0.0 $ Prelude.zip v1 v2
        mag v       = sqrt $ sum (Prelude.map (** 2) v)


userProfile :: SNGraph -> Vertex -> [Float]
userProfile (g, vm, wm, pm) u = Prelude.map normWeight ls
    where
        vs = vertices g
        ls = Prelude.filter ((Location ==) . (vm !)) vs
        lu = visited u

        isUser v = (vm ! v) == User

        isFriend e@(v1, v2) | e `Map.member` wm = isUser v1 && isUser v2
                            | otherwise         = False

        visited v = Prelude.filter (\v' -> not $ isFriend (v, v')) $ (Array.!) g v

        normWeight l | (u, l) `Map.member` wm = (wm ! (u, l)) / sum (Prelude.map (\l' -> wm ! (u, l')) lu)
                     | otherwise              = 0.0


lfbca :: SNGraph -> Vertex -> Float -> Float -> Int -> PSQ Vertex Float
lfbca sng@(g, vm, wm, pm) u d β = fbca (g', vm, wm'', pm) u d
    where
        vs = vertices g
        es = edges g
        us = Prelude.filter isUser vs

        isUser v = (vm ! v) == User

        isFriend e@(v1, v2) | e `Map.member` wm = isUser v1 && isUser v2
                            | otherwise         = False

        visited v = Prelude.filter (\v' -> not $ isFriend (v, v')) $ (Array.!) g v

        sharesLoc v1 v2 = not $ List.null (visited v1 `List.intersect` visited v2)

        userSim v1 v2 = cosineSim (userProfile (g', vm, wm', pm) v1) (userProfile (g', vm, wm', pm) v2)

        lfes = [(u1, u2) | u1 <- us, u2 <- us, u1 /= u2 && sharesLoc u1 u2] List.\\ es
        g'   = buildG (0, length vs - 1) $ es ++ lfes
        wm'  = wm `Map.union` Map.fromList (Prelude.zip lfes [-1.0..])
        wm'' = Map.mapWithKey (\(v1, v2) w -> if isUser v1 && isUser v2 then augWeight v1 v2 else w) wm'

        augWeight v1 v2 =
            case () of
                _ | v2 `Set.member` (friends Set.\\ locFriends)             -> (1 - β) / fromIntegral (Set.size friends)
                  | v2 `Set.member` (friends `Set.intersection` locFriends) -> (1 - β) / fromIntegral (Set.size friends) + β / su * userSim v1 v2
                  | v2 `Set.member` (locFriends Set.\\ friends)             -> β / su * userSim v1 v2
            where
                friends    = Set.fromList $ Prelude.filter (\v' -> isFriend (v1, v')) $ (Array.!) g v1
                locFriends = Set.fromList $ Prelude.filter (sharesLoc v1) $ us List.\\ [v1]
                su         = sum $ Set.map (userSim v1) locFriends


-- Graph visualization utils

data Graphv a n = Graphv (Array.Array Int [Int]) (Array.Array Int [Int]) (Map (a, a) n) (Bimap.Bimap a Int)

instance (Ord a, Show a, Show n, Fractional n) => Show (Graphv a n) where
    
    show g@(Graphv _ _ wmap vmap) = "graph G {\n" 
                                ++ concatMap (\((u, v), w) -> show (vmap Bimap.! u) ++ " -- " ++ show (vmap Bimap.! v) ++ "[label=\"" ++ show w ++ "\"];\n") wes 
                                ++ intercalate "\n" (Prelude.map (\u -> show (vmap Bimap.! u) ++ " [label=\"" ++ (escape . show) u ++ "\"];") $ verticesv g)
                                ++ "\n}"
        where
            wes = Prelude.map (\e -> (e, wmap Map.! e)) $ edgesv g

            escape []         = []
            escape ('"' : s') = '\\' : '"' : escape s'
            escape (c : s')   = c : escape s'


outAdj :: (Ord a, Fractional n) => Graphv a n -> a -> [a]
outAdj (Graphv oarr _ _ vmap) = Prelude.map (vmap Bimap.!>) . (Array.!) oarr . (Bimap.!) vmap


inAdj :: (Ord a, Fractional n) => Graphv a n -> a -> [a]
inAdj (Graphv _ iarr _ vmap) = Prelude.map (vmap Bimap.!>) . (Array.!) iarr . (Bimap.!) vmap


edgesv :: (Ord a, Fractional n) => Graphv a n -> [(a, a)]
edgesv (Graphv arr _ _ vmap) = Prelude.map (bimap (vmap Bimap.!>) (vmap Bimap.!>)) $ Prelude.foldr (\(v, vs) es -> Prelude.foldr (\u es' -> (v, u) : es') es vs) [] $ Array.assocs arr


verticesv :: (Ord a, Fractional n) => Graphv a n -> [a]
verticesv (Graphv arr _ _ vmap) = Prelude.map (vmap Bimap.!>) $ Array.indices arr


mkgraph :: (Ord a, Fractional n) => [((a, a), n)] -> Graphv a n
mkgraph wes = Graphv (mkarray mappedes) (mkarray $ Prelude.map swap mappedes) wmap vmap
    where
        (es, ws) = Prelude.unzip wes
        mappedes = Prelude.map (bimap (vmap Bimap.!) (vmap Bimap.!)) es
        vs       = let (from, to) = Prelude.unzip es in Prelude.foldr (List.union . (: [])) [] $ from ++ to
        vmap     = Bimap.fromList $ Prelude.zip vs [1, 2..]
        wmap     = Map.fromList wes
        mkarray  = Array.accumArray (flip (:)) [] (1, length vs)


addScores :: (Ord a, Eq a, Fractional n) => Graphv a n -> Map a Float -> Graphv (a, Float) n
addScores g@(Graphv _ _ wmap _) scoreMap = mkgraph $ Prelude.map addScore es
    where
        addScore e@(u, v) = (((u, uScore), (v, vScore)), wmap Map.! e)
            where
                uScore = if Map.member u scoreMap then scoreMap Map.! u else -1 -- no rank
                vScore = if Map.member v scoreMap then scoreMap Map.! v else -1 -- no rank

        es = edgesv g


visualExampleGraph = mkgraph [(("l1", "u1"), 7.0), (("l2", "u1"), 3.0), (("l2", "u3"), 1.0), (("l3", "u3"), 1.0), (("u1", "u3"), -1.0), (("u3", "u2"), -1.0), (("l1", "u4"), 2.0)]

vertexToVisual v = 
    case v of
        0 -> "l1"
        1 -> "u1"
        2 -> "l2"
        3 -> "l3"
        4 -> "u3"
        5 -> "u2"
        6 -> "u4"


pqToMap :: PSQ Vertex Float -> Map String Float
pqToMap pq = Map.fromList $ Prelude.map (\binding -> (vertexToVisual (PQ.key binding), PQ.prio binding)) (PQ.toList pq)


convertGtoVG :: SNGraph -> Graphv String Float
convertGtoVG sng@(g, _, wm, _) = mkgraph wes
    where
        es  = edges g
        es' = Prelude.foldr (\(u, v) acc -> if (u, v) `Set.member` acc || (v, u) `Set.member` acc then acc else Set.insert (u, v) acc) Set.empty es
        wes = Prelude.map (\e@(u, v) -> ((vertexToVisual u, vertexToVisual v), wm ! e)) $ Set.toList es'


lfbcaGraph :: SNGraph -> Vertex -> Float -> Float -> SNGraph
lfbcaGraph sng@(g, vm, wm, pm) u d β = (g', vm, wm'', pm)
    where
        vs = vertices g
        es = edges g
        us = Prelude.filter isUser vs

        isUser v = (vm ! v) == User

        isFriend e@(v1, v2) | e `Map.member` wm = isUser v1 && isUser v2
                            | otherwise         = False

        visited v = Prelude.filter (\v' -> not $ isFriend (v, v')) $ (Array.!) g v

        sharesLoc v1 v2 = not $ List.null (visited v1 `List.intersect` visited v2)

        userSim v1 v2 = cosineSim (userProfile (g', vm, wm', pm) v1) (userProfile (g', vm, wm', pm) v2)

        lfes = [(u1, u2) | u1 <- us, u2 <- us, u1 /= u2 && sharesLoc u1 u2] List.\\ es
        g'   = buildG (0, length vs - 1) $ es ++ lfes
        wm'  = wm `Map.union` Map.fromList (Prelude.zip lfes [-1.0..])
        wm'' = Map.mapWithKey (\(v1, v2) w -> if isUser v1 && isUser v2 then augWeight v1 v2 else w) wm'

        augWeight v1 v2 =
            case () of
                _ | v2 `Set.member` (friends Set.\\ locFriends)             -> (1 - β) / fromIntegral (Set.size friends)
                  | v2 `Set.member` (friends `Set.intersection` locFriends) -> (1 - β) / fromIntegral (Set.size friends) + β / su * userSim v1 v2
                  | v2 `Set.member` (locFriends Set.\\ friends)             -> β / su * userSim v1 v2
            where
                friends    = Set.fromList $ Prelude.filter (\v' -> isFriend (v1, v')) $ (Array.!) g v1
                locFriends = Set.fromList $ Prelude.filter (sharesLoc v1) $ us List.\\ [v1]
                su         = sum $ Set.map (userSim v1) locFriends