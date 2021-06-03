module Coloring
    ( bca
    , fbca
    , lfbca
    , exampleGraph
    , fromPQOrdered     
    ) where

import Data.Graph as Graph
import Data.Map as Map
import Data.Set as Set hiding((!)) 
import Data.List as List
import qualified Data.Array as Array
import Data.PSQueue as PQ
import Data.Maybe as Maybe


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