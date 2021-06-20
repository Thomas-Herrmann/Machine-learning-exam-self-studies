module Summary
( summary
, transitionProba
, clusteredTransitionProba
, segmentSentences
, kmeans
, Summary(..)
) where

import Data.List(intercalate, sortBy, elemIndex)
import Data.Map as Map
import Data.Matrix as Matrix hiding ((!))
import Data.Vector as Vector (foldr)
import Data.Maybe(fromJust)


newtype Summary = Summary [(String, Float)]

instance Show Summary where
    show (Summary l) = intercalate "\n" $ zipWith (\i (s, r) -> show i ++ ": " ++ s ++ " -- " ++ show r) [1, 2 .. ] l


type WordMap = Map String Float


summary :: (String -> String -> [String] -> Float) -> Maybe Int -> String -> Summary
summary transprobaf m s = 
    case m of
        Just k  -> Summary $ Prelude.take k (orderByRank sentences transprobaf)
        Nothing -> Summary $ orderByRank sentences transprobaf
    where
        sentences = segmentSentences s


segmentSentences :: String -> [String]
segmentSentences s = aux False s [[]]
    where
        aux _ [] []           = []
        aux _ [] ([] : l)     = l
        aux _ [] (buffer : l) = reverse buffer : l

        aux _ ('[' : s) l    = aux True s l
        aux True (']' : s) l = aux False s l
        aux True (_ : s) l   = aux True s l

        aux False (e : s) l@([] : _)   | isWhitespace e                    = aux False s l
        aux False (c : s) l@([] : _)   | isSentenceEnd c || isWhitespace c = aux False s l 
        aux False (e : s) (buffer : l) | isSentenceEnd e                   = aux False s $ [] : reverse (e : buffer) : l

        aux False ('\n' : s) l                                         = aux False s l
        aux False ('e' : 't' : ' ' : 'a' : 'l' : '.' : s) (buffer : l) = aux False s $ ('e' : 't' : ' ' : 'a' : 'l' : '.' : buffer) : l
        aux False (c : s) (buffer : l)                                 = aux False s $ (c : buffer) : l

        aux _ s l = error "invalid pattern"


segmentWords :: String -> [String]
segmentWords s = aux s [[]]
    where
        aux [] [] = []
        aux [] ([] : l)     = l
        aux [] (buffer : l) = reverse buffer : l

        aux (c : s) l@([] : _)       | isSentenceEnd c || isWhitespace c = aux s l 
        aux (e : w : s) (buffer : l) | isSentenceEnd e && isWhitespace w = aux s $ [] : reverse buffer : l
        aux (c : s) (buffer : l)     | isSentenceEnd c || isWhitespace c = aux s $ [] : reverse buffer : l

        aux ('"' : s) l = aux s l
        aux (',' : s) l = aux s l

        aux (c:s) (buffer : l) = aux s $ (c : buffer) : l

        aux s l = error "invalid pattern"


isWhitespace :: Char -> Bool
isWhitespace c =
    case c of
        ' '  -> True 
        '\n' -> True
        _    -> False


isSentenceEnd :: Char -> Bool
isSentenceEnd c =
    case c of
        '.'  -> True
        '!'  -> True 
        '?'  -> True 
        _    -> False


orderByRank :: [String] -> (String -> String -> [String] -> Float) -> [(String, Float)]
orderByRank v transprobaf = sortBy (\(_, r1) (_, r2) -> compare r2 r1) $ senScore v transprobaf


cosineSim :: WordMap -> WordMap -> Float
cosineSim wmap1 wmap2 | wmap1 == wmap2 = 0.0 
                      | otherwise      = 
        if wmap1Length == 0.0 || wmap2Length == 0.0 
            then 0.0 
            else wmapProduct / (wmap1Length * wmap2Length)
    where
        wmap1Length = sqrt $ Prelude.foldr ((+) . (** 2)) 0.0 $ Map.elems wmap1
        wmap2Length = sqrt $ Prelude.foldr ((+) . (** 2)) 0.0 $ Map.elems wmap2
        wmapProduct = sum $ Map.elems (Map.intersectionWith (*) wmap1 wmap2)


makeWordMap :: [String] -> WordMap
makeWordMap = Prelude.foldr addWord Map.empty 
    where
        addWord w wmap | w `Map.member` wmap = Map.adjust (+ 1) w wmap 
                       | otherwise           = Map.insert w 1 wmap 


transitionProba :: String -> String -> [String] -> Float
transitionProba s1 s2 v = cosineSim wmap1 wmap2 / Prelude.foldr (((+) . cosineSim wmap1) . makeWordMap . segmentWords) 0.0 v
    where
        wmap1 = makeWordMap $ segmentWords s1
        wmap2 = makeWordMap $ segmentWords s2


clusteredTransitionProba :: [WordMap] -> String -> String -> [String] -> Float
clusteredTransitionProba clusters s1 s2 v = clusteredSim wmap1 wmap2 / Prelude.foldr (((+) . clusteredSim wmap1) . makeWordMap . segmentWords) 0.0 v
    where
        wmap1    = makeWordMap $ segmentWords s1
        wmap2    = makeWordMap $ segmentWords s2
        docwmap  = makeWordMap $ segmentWords (unwords v)

        lambda = 0.5

        clusterImpor wmap        = cosineSim (assignCluster clusters wmap) docwmap
        clusterSim wmap          = cosineSim wmap $ assignCluster clusters wmap
        clusteredSim wmap1 wmap2 = cosineSim wmap1 wmap2 * lambda * clusterImpor wmap1 * clusterSim wmap1 + cosineSim wmap1 wmap2 * (1.0 - lambda) * clusterImpor wmap2 * clusterSim wmap2


makeTransitionMatrix :: [String] -> (String -> String -> [String] -> Float) -> Matrix Float
makeTransitionMatrix v transprobaf = makeStochastic tmat 1
    where 
        ns   = length v
        tmat = Matrix.matrix ns ns $ \(i, j) -> transprobaf (v !! (i - 1)) (v !! (j - 1)) v

        makeStochastic m i | i > ns      = m
                           | zeroRow m i = makeStochastic (Matrix.mapRow (\_ _ -> 1.0 / fromIntegral ns) i m) $ i + 1
                           | otherwise   = makeStochastic m $ i + 1

        zeroRow :: Matrix Float -> Int -> Bool
        zeroRow m i = Vector.foldr (+) 0.0 (Matrix.getRow i m) == 0.0
 

senScore :: [String] -> (String -> String -> [String] -> Float) -> [(String, Float)]
senScore v transprobaf = Prelude.zip v (Matrix.toList $ aux e)
    where
        mu = 0.85
        m  = makeTransitionMatrix v transprobaf
        e  = matrix (length v) 1 $ const 1.0

        aux lambda | minChange < threshold = lambda'
                   | otherwise             = aux lambda' 
            where
                lambda'   = scaleMatrix mu (transpose m) * lambda + scaleMatrix ((1.0 - mu) / fromIntegral (length v)) e
                minChange = minimum $ Matrix.elementwise (\r1 r2 -> abs (r1 - r2)) lambda' lambda
                threshold = 0.00001


kmeans :: [String] -> Int -> [WordMap]
kmeans v k = aux $ Prelude.take k wmaps
    where
        wmaps = Prelude.map (makeWordMap . segmentWords) v

        aux :: [WordMap] -> [WordMap]
        aux clusters | clusters' == clusters = clusters
                     | otherwise             = aux clusters'
            where
                assignedClusters :: Map WordMap [WordMap]
                assignedClusters = Prelude.foldr (\wmap -> Map.adjust (wmap :) $ assignCluster clusters wmap) (Map.fromList [(c, []) | c <- clusters]) wmaps 
                clusters'        = Prelude.map (\l -> Map.map (/ fromIntegral (length l)) $ Map.unionsWith (+) l) $ Map.elems assignedClusters


assignCluster :: [WordMap] -> WordMap -> WordMap
assignCluster clusters wmap = clusters !! fromJust (maximum ranks `elemIndex` ranks)
    where
        ranks = Prelude.map (cosineSim wmap) clusters