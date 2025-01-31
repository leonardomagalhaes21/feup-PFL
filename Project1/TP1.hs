import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


-- Type definitions
type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City, City, Distance)]

-- 1. cities :: RoadMap -> [City]
-- Extracts a list of unique cities from a given roadmap.
-- roadmap - A list of tuples representing the roadmap.
cities :: RoadMap -> [City]
cities roadmap = Data.List.nub [city | (city1, city2, _) <- roadmap, city <- [city1, city2]]

-- 2. areAdjacent :: RoadMap -> City -> City -> Bool
-- Checks if two cities are directly connected in a roadmap.
-- roadmap - A list of tuples representing the roadmap.
-- city1 - The first city.
-- city2 - The second city.
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 =
  any isConnected roadmap
  where
    isConnected (c1, c2, _) = (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)

-- 3. distance :: RoadMap -> City -> City -> Maybe Distance
-- Calculates the distance between two cities in a roadmap.
-- roadmap - A list of tuples representing the roadmap.
-- city1 - The first city.
-- city2 - The second city.
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2 =
  if areAdjacent roadmap city1 city2 then Just d else Nothing
  where
    d = minimum [d | (c1, c2, d) <- roadmap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)]

-- 4. adjacent :: RoadMap -> City -> [(City, Distance)]
-- Returns a list of adjacent cities and their distances from a city in a roadmap.
-- roadmap - A list of tuples representing the roadmap.
-- city - The city we want to find the adjacent cities.
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent roadmap city =
  [(c2, d) | (c1, c2, d) <- roadmap, c1 == city] ++ [(c1, d) | (c1, c2, d) <- roadmap, c2 == city]

-- 5. pathDistance :: RoadMap -> Path -> Maybe Distance
-- Calculates the total distance of a path in a roadmap.
-- roadmap - A list of tuples representing the roadmap.
-- path - A list of cities representing the path.

-- Helper function to add two Maybe Distances
addMaybe :: Maybe Distance -> Maybe Distance -> Maybe Distance
addMaybe Nothing _ = Nothing
addMaybe _ Nothing = Nothing
addMaybe (Just x) (Just y) = Just (x + y)

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0    -- empty path
pathDistance _ [_] = Just 0   -- single city
pathDistance roadmap (c1:c2:cs) = addMaybe (distance roadmap c1 c2) (pathDistance roadmap (c2:cs))

-- 6. rome :: RoadMap -> [City]
-- Returns the list of cities with the highest degree in a roadmap.
-- roadmap - A list of tuples representing the roadmap.

-- Helper function to calculate the degree of a city
degree :: RoadMap -> City -> Int
degree roadmap city = length [(c1, c2, d) | (c1, c2, d) <- roadmap, c1 == city || c2 == city]

rome :: RoadMap -> [City]
rome roadmap =
  let citiesList = cities roadmap
      degrees = [(city, degree roadmap city) | city <- citiesList]
      maxDegree = maximum (map snd degrees)
  in if null degrees then [] else [city | (city, d) <- degrees, d == maxDegree]


-- 7. isStronglyConnected :: RoadMap -> Bool
-- Checks if a graph is strongly connected.
-- roadmap - A list of tuples representing the roadmap.

-- Depth-First-Search helper function
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadmap city visited
  | city `elem` visited = visited -- already visited
  | otherwise = foldl (\acc (c, _) -> dfs roadmap c acc) (city : visited) (adjacent roadmap city) -- visit adjacent cities

-- Reverses the edges in the graph
reverseEdges :: RoadMap -> RoadMap
reverseEdges = map (\(c1, c2, d) -> (c2, c1, d))

-- Checks if the graph is strongly connected
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap =
  let citiesList = cities roadmap
      cityNum = length citiesList
      visited = dfs roadmap (head citiesList) [] -- DFS on the original graph
      visitedReversedEdges = dfs (reverseEdges roadmap) (head citiesList) [] -- DFS on the graph with reversed edges
  in (cityNum == 0 || length visited == cityNum && length visitedReversedEdges == cityNum)


-- 8. shortestPath :: RoadMap -> City -> City -> [Path]
-- Finds the shortest path between two cities in a roadmap.
-- roadmap - A list of tuples representing the roadmap.
-- c1 - The starting city.
-- c2 - The destination city.

type Queue = [(Path, Distance)]
bfs :: RoadMap -> City -> Queue -> [(Path, Distance)] -> [(Path, Distance)]
bfs _ _ [] paths = paths -- empty queue
bfs roadmap goal ((path, dist):restQueue) paths =
  let
    currentCity = head path
    adjCities = [(city, d) | (city, d) <- adjacent roadmap currentCity, city `notElem` path]
    adjPaths = [(city : path, dist + d) | (city, d) <- adjCities]
  in
    if currentCity == goal
    then bfs roadmap goal restQueue ((reverse path, dist) : paths) -- add path to the list of paths
    else bfs roadmap goal (restQueue ++ adjPaths) paths -- add adjacent paths to the queue

-- BFS to find all paths
bfsPaths :: RoadMap -> City -> City -> [(Path, Distance)]
bfsPaths roadmap startCity goal = bfs roadmap goal [([startCity], 0)] []

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap c1 c2 =
  let allPaths = bfsPaths roadmap c1 c2
      minDist = minimum (map snd allPaths)
  in [path | (path, d) <- allPaths, d == minDist]


-- 9. travelSales :: RoadMap -> Path
-- Finds the shortest path that visits all cities in a roadmap and returns to the start.
-- roadmap - A list of tuples representing the roadmap.

bfsTsp :: RoadMap -> [City] -> Queue -> [(Path, Distance)]
bfsTsp _ _ [] = [] -- empty queue
bfsTsp roadmap citiesList ((path, dist):restQueue) =
    let
      currentCity = head path
      adjCities = [(city, d) | (city, d) <- adjacent roadmap currentCity, city `notElem` path]
      adjPaths = [(city : path, dist + d) | (city, d) <- adjCities]

      visitedCities = Data.List.nub path
    in
      if length visitedCities == length citiesList
      then (reverse path, dist) : bfsTsp roadmap citiesList restQueue -- add complete path to the list of paths
      else bfsTsp roadmap citiesList (restQueue ++ adjPaths) -- add adjacent paths to the queue

minCostPath :: [(Path, Distance)] -> (Path, Distance)
minCostPath paths = foldl mini (head paths) (tail paths)
  where
    mini (p1, d1) (p2, d2) = if d1 < d2 then (p1, d1) else (p2, d2)

findPathWithReturn :: RoadMap -> City -> Maybe (Path, Distance)
findPathWithReturn roadmap startCity =
    let 
      citiesList = cities roadmap
      possiblePaths = bfsTsp roadmap citiesList [([startCity], 0)]
      validPaths = [(path, dist) | (path, dist) <- possiblePaths, head path == startCity, areAdjacent roadmap (head path) (last path)] -- paths that return to the start city
    in
      if null validPaths then Nothing else Just $ minCostPath validPaths -- return the minimum cost path

travelSales :: RoadMap -> Path
travelSales roadmap =
    let 
      citiesList = cities roadmap
      startCity = head citiesList
      res = findPathWithReturn roadmap startCity
    in case res of
      Just (path, _) -> path ++ [startCity] -- add the start city to the end
      Nothing -> []

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined

-- Test Graphs
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]


{-
-- Property-based Testing
runTests :: IO ()
runTests = do
  -- 1. 'cities' pbt
  quickCheck $ \(roadmap :: RoadMap) ->
    let allCities = [c | (c1, c2, _) <- roadmap, c <- [c1, c2]]
    in all (`elem` cities roadmap) allCities

  -- 2. 'areAdjacent' pbt
  quickCheck $ \(roadmap :: RoadMap) (city1 :: City) (city2 :: City) ->
    areAdjacent roadmap city1 city2 == any (\(c1, c2, _) -> c1 == city1 && c2 == city2 || c1 == city2 && c2 == city1) roadmap

  -- 3. 'distance' pbt
  quickCheck $ \(roadmap :: RoadMap) (city1 :: City) (city2 :: City) ->
    let dist = distance roadmap city1 city2
    in case dist of
         Just _  -> areAdjacent roadmap city1 city2 -- adjacent
         Nothing -> not (areAdjacent roadmap city1 city2) -- not adjacent

  -- 4. 'adjacent' pbt
  quickCheck $ \(roadmap :: RoadMap) (city :: City) ->
    all (\(c, _) -> areAdjacent roadmap city c) (adjacent roadmap city)

  -- 5. 'pathDistance' pbt
  quickCheck $ \(roadmap :: RoadMap) (path :: Path) ->
    case pathDistance roadmap path of
      Just dist -> 
        let adjDistances = [d | (c1, c2) <- zip path (tail path), -- adjacent cities
                                Just d <- [distance roadmap c1 c2]] 
        in dist == sum adjDistances
      Nothing -> True -- invalid path

  -- 6. 'rome' pbt
  quickCheck $ \(roadmap :: RoadMap) ->
    let maxDegreeCities = rome roadmap
        maxDegree = maximum $ map (degree roadmap) (cities roadmap)
    in all (\city -> degree roadmap city == maxDegree) maxDegreeCities

  -- 7. 'isStronglyConnected' pbt
  quickCheck $ \(roadmap :: RoadMap) ->
    let allCities = cities roadmap
        cityNum = length allCities
    in all (\c -> length (dfs roadmap c []) == cityNum) allCities == isStronglyConnected roadmap

  -- 8. 'shortestPath' pbt
  quickCheck $ \(roadmap :: RoadMap) (c1 :: City) (c2 :: City) ->
    let paths = shortestPath roadmap c1 c2
        allPaths = bfsPaths roadmap c1 c2
        minDist = minimum (map snd allPaths)
    in all (\path -> pathDistance roadmap path == Just minDist) paths

  -- 9. 'travelSales' pbt
  quickCheck $ \(roadmap :: RoadMap) ->
    let
      tspPath = travelSales roadmap
      allCities = cities roadmap
      isConnected = isStronglyConnected roadmap
      isTourValid =
        length tspPath == length allCities + 1            -- make sure the path returns to the start city
        && head tspPath == last tspPath                   -- path starts and ends at the same city
        && Data.List.nub (init tspPath) == allCities      -- each city appears exactly once (except the start city)
    in
      if null allCities 
      then null tspPath  -- empty graph
      else (isConnected && isTourValid) || (not isConnected && null tspPath) -- if the graph is not connected, there is no path

-}