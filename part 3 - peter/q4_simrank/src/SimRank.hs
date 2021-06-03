module SimRank
( webGraph
, personItemGraph
, squareG
, edges
, vertices
, homoSimRank
, bipartiteSimRank
, Graph(..)
) where


import Data.Map as Map
import Data.Bimap as Bimap
import Data.Set as Set hiding((!)) 
import Data.List as List
import Data.Array as Array
import Data.Maybe(fromJust)
import Data.Bifunctor(bimap)
import Data.Ord(Ordering(..))
import Data.Tuple(swap)


data Graph a n = Graph (Array Int [Int]) (Array Int [Int]) (Map (a, a) n) (Bimap a Int)


data BiPair a = BiPair a a


instance Eq a => Eq (BiPair a) where

    BiPair s t == BiPair s' t' = (s == s' && t == t') || (s == t' && t == s')


instance (Ord a, Show a) => Show (BiPair a) where

    show (BiPair s t) | s <= t    = "{" ++ show s ++ ", " ++ show t ++ "}"
                      | otherwise = "{" ++ show t ++ ", " ++ show s ++ "}"


instance (Eq a, Ord a) => Ord (BiPair a) where

    p@(BiPair s t) `compare` p'@(BiPair s' t') | p == p'   = EQ
                                               | s == s'   = t `compare` t'
                                               | t == t'   = s `compare` s'
                                               | s == t'   = t `compare` s'
                                               | t == s'   = s `compare` t'
                                               | otherwise = min s t `compare` min s' t'


instance (Ord a, Show a, Show n, Num n) => Show (Graph a n) where
    
    show g@(Graph _ _ wmap vmap) = "digraph G {\n" 
                                ++ concatMap (\((u, v), w) -> show (vmap Bimap.! u) ++ " -> " ++ show (vmap Bimap.! v) ++ "[label=\"" ++ show w ++ "\"];\n") wes 
                                ++ intercalate "\n" (Prelude.map (\u -> show (vmap Bimap.! u) ++ " [label=\"" ++ (escape . show) u ++ "\"];") $ vertices g)
                                ++ "\n}"
        where
            wes = Prelude.map (\e -> (e, wmap Map.! e)) $ edges g

            escape []         = []
            escape ('"' : s') = '\\' : '"' : escape s'
            escape (c : s')   = c : escape s' 


data WebSpace = Univ | ProfA | ProfB | StudentA | StudentB deriving (Show, Ord, Eq)

data Person = PersonA | PersonB deriving (Show, Ord, Eq)

data Item = Sugar | Frosting | Eggs | Flour deriving (Show, Ord, Eq)


webGraph :: Graph WebSpace Int
webGraph = mkgraph $ Prelude.zip [ (Univ, ProfA), (Univ, ProfB)
                                 , (ProfA, StudentA), (ProfB, StudentB)
                                 , (StudentA, Univ), (StudentB, ProfB)
                                 ] [1, 1..]


personItemGraph :: Graph (Either Person Item) Int
personItemGraph = mkgraph $ Prelude.zip [ (Left PersonA, Right Sugar), (Left PersonA, Right Frosting)
                                        , (Left PersonA, Right Eggs), (Left PersonB, Right Frosting)
                                        , (Left PersonB, Right Eggs), (Left PersonB, Right Flour)
                                        ] [1, 1..]


outAdj :: (Ord a, Num n) => Graph a n -> a -> [a]
outAdj (Graph oarr _ _ vmap) = Prelude.map (vmap !>) . (Array.!) oarr . (Bimap.!) vmap


inAdj :: (Ord a, Num n) => Graph a n -> a -> [a]
inAdj (Graph _ iarr _ vmap) = Prelude.map (vmap !>) . (Array.!) iarr . (Bimap.!) vmap


edges :: (Ord a, Num n) => Graph a n -> [(a, a)]
edges (Graph arr _ _ vmap) = Prelude.map (bimap (vmap !>) (vmap !>)) $ Prelude.foldr (\(v, vs) es -> Prelude.foldr (\u es' -> (v, u) : es') es vs) [] $ Array.assocs arr


vertices :: (Ord a, Num n) => Graph a n -> [a]
vertices (Graph arr _ _ vmap) = Prelude.map (vmap !>) $ Array.indices arr


mkgraph :: (Ord a, Num n) => [((a, a), n)] -> Graph a n
mkgraph wes = Graph (mkarray mappedes) (mkarray $ Prelude.map swap mappedes) wmap vmap
    where
        (es, ws) = Prelude.unzip wes
        mappedes = Prelude.map (bimap (vmap Bimap.!) (vmap Bimap.!)) es
        vs       = let (from, to) = Prelude.unzip es in Prelude.foldr (List.union . (: [])) [] $ from ++ to
        vmap     = Bimap.fromList $ Prelude.zip vs [0, 1..]
        wmap     = Map.fromList wes
        mkarray  = Array.accumArray (flip (:)) [] (0, length vs - 1)


squareG :: (Ord a, Num n) => Graph a n -> Graph (BiPair a) n
squareG g@(Graph arr _ wmap vmap) = mkgraph $ (prune . toBiPairs) [(((v, u), (v', u')), wmap Map.! e1 * wmap Map.! e2) | e1@(v, v') <- es, e2@(u, u') <- es, e1 /= e2]
    where
        es        = edges g
        toBiPairs = Prelude.map $ \(((v, u), (v', u')), w) -> ((BiPair v u, BiPair v' u'), w)

prune :: Eq a => [((BiPair a, BiPair a), n)] -> [((BiPair a, BiPair a), n)]
prune es = aux es []
    where
        aux [] acc = acc
        aux (we : wes) acc | we `exists` acc = aux wes acc
                           | otherwise       = aux wes $ we : acc

        exists (e, _) []                = False
        exists we@(e, _) ((e', _) : es) = e == e' || exists we es  


homoSimRank :: (Ord a, Eq a, Num n) => Float -> Graph a n -> Int -> Map (BiPair a) Float
homoSimRank = simRank inAdj 


bipartiteSimRank :: (Ord a, Eq a, Ord b, Eq b, Num n) => Float -> Graph (Either a b) n -> Int -> Map (BiPair (Either a b)) Float
bipartiteSimRank = simRank $ 
    \g e -> 
        case e of
            Left _ -> g `outAdj` e
            _      -> g `inAdj` e


simRank :: (Ord a, Eq a, Num n) => (Graph a n -> a -> [a]) -> Float -> Graph a n -> Int -> Map (BiPair a) Float
simRank adjf c g k = aux rinit k
    where
        vs    = vertices g
        rinit = Map.fromList [(BiPair u v, fromIntegral . fromEnum $ u == v) | u <- vs, v <- vs]

        aux r k | k <= 0    = r
                | otherwise = aux (Map.mapWithKey (\e _ -> rank e r) r) $ k - 1
        
        rank (BiPair u v) r | u == v    = 1
                            | otherwise =
            if List.null inu || List.null inv
                then 0
                else c * Prelude.foldr (\u' -> (+) $ Prelude.foldr ((+) . (r Map.!) . BiPair u') 0 inv) 0 inu / fromIntegral (length inu * length inv) 
            where
                inu = g `adjf` u
                inv = g `adjf` v
                