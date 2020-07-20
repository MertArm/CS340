-- 1. A Star Search
-- 2. Data Type
-- 3. Goal function (when have I won?)
-- 4. Successor function (where can I go from this step?)
-- 5. Heuristic function (how far away, at minimum, am I from my goal?)

import Data.Char
import Data.Ord
import Data.List
import Debug.Trace

search :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> ([a] -> [a] -> [a])
  -> [a] -> [a] -> Maybe a
search goal succ comb nodes oldNodes
  | null nodes = Nothing
  | goal (head nodes) = Just (head nodes)
  | otherwise = let (n:ns) = nodes
                in traceShow (n,nodes) $ search goal succ comb
                   (comb (removeDups (succ n)) ns)
                   (adjoin n oldNodes)
  where removeDups = filter (not . ((flip elem) (nodes ++ oldNodes)))
        adjoin x lst = if elem x lst then lst else x:lst

bestFirstSearch :: (Eq a, Show a, Ord b) => (a -> Bool) -> (a -> [a])
                -> (a -> b) -> a -> Maybe a
bestFirstSearch goal succ score start = search goal succ comb [start] []
  where comb new old = sortOn score (new ++ old)

data Location = Location {
                locName :: String,
                latitude :: Double,
                longitude :: Double
                } deriving (Eq)

data Path = Path {
            totalDist :: Double,
            route :: [Location]
            } deriving (Eq)

{-
data Location = Location String Double Double deriving Show
data Path = Path [(Double,Double)] deriving Eq
-}

geoDistance :: (Double,Double) -> (Double,Double) -> Double
geoDistance (lat1,lon1) (lat2,lon2)
  = 3959 * (acos (sindeg(lat1) * sindeg(lat2)
                  + cosdeg(lat1) * cosdeg(lat2) * cosdeg(lon2-lon1)))
    where sindeg = sin . toRadian
          cosdeg = cos . toRadian
          toRadian deg = pi/180.0 * deg

distanceBetween :: Location -> Location -> Double
distanceBetween (Location _ lat1 lon1) 
                (Location _ lat2 lon2) = geoDistance (lat1, lon1) 
                                                     (lat2, lon2)

findPath :: [Location] -> String -> String -> Double -> Maybe Path
findPath loc start goal succ =
  bestFirstSearch (\(Path (l:_)) -> l == succ)
                  nextPaths scorePath $ Path [start]
  where nextPaths (Path (l:ls)) = map (\nl -> Path $ nl:p)
                                       $ filter (not . (flip elem) p)
                                       $ distanceBetween m l
        scorePath (Path (l:ls)) = geoDistance l succ

lat_long_data = [ ("Bangkok", 13.76, 100.5),
                  ("Beijing", 39.90, 116.41),
                  ("Buenos Aires", -34.6, -58.38),
                  ("Chicago", 41.87, -87.63),
                  ("Dallas", 32.77, -96.79),
                  ("Denver", 39.73, -104.99),
                  ("Dubai", 25.20, 55.27),
                  ("Honolulu", 21.30, -157.85),
                  ("London", 51.50, -0.12),
                  ("Los-Angeles", 34.05, -118.24),
                  ("New-York", 40.71, -74.00),
                  ("Paris", 48.85, 2.35) ]

testAirports :: [Location]
testAirports = map (\(n,lat,lon)->Location n lat lon) lat_long_data