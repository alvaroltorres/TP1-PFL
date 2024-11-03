import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set


-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

{-- The cities function takes as argument a RoadMap 
and returns a List indicating all the cities in that Roadmap.--}
cities :: RoadMap -> [City]
-- queremos retornar a primeira de cada tuplo se não estiver na lista
-- queremos retornar a segunda de cada tuplo se não estiver
-- retornar ambas se nenhuma estiver
-- senão (como ambas estão na lista), não se adiciona
cities [] = []
cities roadmap = citiesHelperFunction [] roadmap

citiesHelperFunction :: [City] -> RoadMap -> [City]
citiesHelperFunction cidades [] = cidades
citiesHelperFunction cidades (x:xs) 
        | elem a cidades && elem b cidades = citiesHelperFunction cidades xs
        | elem a cidades && not (elem b cidades) = citiesHelperFunction (b:cidades) xs
        | not (elem a cidades) && elem b cidades = citiesHelperFunction (a:cidades) xs
        | otherwise = citiesHelperFunction (a:b:cidades) xs
        where (a,b,c) = x

{-- The areAdjacent function takes as arguments a RoadMap and two Cities 
and returns a Bool indicating whether two cities are linked directly.--}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((a,b,_):xs) c1 c2
    | a == c1 && b == c2 = True
    | a == c2 && b == c1 = True
    | otherwise = areAdjacent xs c1 c2

{-- The distance function takes as arguments a RoadMap 
and two cities and returns Nothing if there isn't a direct road between
the cities or the distance between them if some road exists. --}
distance :: RoadMap -> City -> City -> Maybe Distance
-- quero o tuplo em que essa primeira cidade aparece primeiro
distance [] a b = Nothing
distance ((x, y, z):xs) a b 
    | ((a == x) && b == y) || (b == x && a == y) = Just z
    | otherwise = distance xs a b

{-- The adjacent function takes as arguments a RoadMap and a City 
and returns a List of (City,Distance) Tuples. The goal of this function is to return the cities adjacent to the given city
and the respective distance between them.--}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((a,b,d):xs) c1
    | a == c1 = (b, d) : adjacent xs c1
    | b == c1 = (a, d) : adjacent xs c1
    | otherwise = adjacent xs c1

{-- The pathDistance function takes as arguments a RoadMap 
and a Path (list of cities) and returns the distance between them if there 
is a path between them in that roadmap (otherwise return Nothing).--}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing -- se não existir roadmap
pathDistance _ [] = Nothing -- se não houver cidades
pathDistance _ [x] = Just 0 -- se houver apenas uma cidade no path (destino, final da recursão)
pathDistance roadmap (a:b:xs) = 
    case distance roadmap a b of
        Nothing -> Nothing -- se não existir caminho direto entre a, b, retorno
        Just dist -> case pathDistance roadmap (b:xs) of -- se existir, vemos se no resto do path existem conexões
                Nothing -> Nothing -- se algures não existir conexão direta, retorno
                Just remainingDist -> Just (dist + remainingDist) -- se tudo correr bem, soma-se os valores dos paths intermediários

{-- The rome function takes as argument a RoadMap and returns 
a List of Cities with the highest number of roads connecting to them.
It uses an auxiliary function cityDegrees that counts the number of connections for each city, taking as arguments a RoadMap and returning a List of (City,Int) Tuples.--}
cityDegrees :: RoadMap -> [(City, Int)]
cityDegrees roadMap = map (\(x:xs) -> (x, length (x:xs))) groupedCities
  where
    groupedCities = Data.List.group $ Data.List.sort allCities
    allCities = concatMap (\(a, b, _) -> [a, b]) roadMap
rome :: RoadMap -> [City]
rome roadMap = map fst (filter (\(_, degree) -> degree == maxDegree) degrees) -- filtrar e retornar as cidades com degree máximo
  where
    degrees = cityDegrees roadMap
    maxDegree = maximum (map snd degrees)

{-- The breadthFirstSearch function takes as arguments a RoadMap and a City and performs a BFS to list all cities reachable from a starting city.--}
breadthFirstSearch :: RoadMap -> City -> [City]
breadthFirstSearch roadMap startCity = bfs [startCity] []
  where
    bfs [] visited = visited
    bfs (current:queue) visited
      | current `elem` visited = bfs queue visited
      | otherwise =
          let neighbors = [neighbor | (neighbor, _) <- adjacent roadMap current]
          in bfs (queue ++ neighbors) (visited ++ [current])

{-- The isStronglyConnected function takes as arguments a RoadMap and checks if the roadMap is strongly connected using Kosaraju's algorithm, returning a Bool, (i.e., if every city is reachable from every other city).--}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap =
    case allCitiesList of
        [] -> True  -- considers an empty graph
        (initialCity:_) ->
            let reachableFromInitial = breadthFirstSearch roadMap initialCity
                reversedRoadMap = [(c2, c1, dist) | (c1, c2, dist) <- roadMap]
                reachableInReversed = breadthFirstSearch reversedRoadMap initialCity
            in length reachableFromInitial == length allCitiesList && length reachableInReversed == length allCitiesList
  where
    allCitiesList = cities roadMap

-- shortestPath function using BFS to find all shortest paths between two cities
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start goal
    | start == goal = [[start]]  -- If the same cities, return the trivial path
    | otherwise     = bfs [[start]] [] Map.empty  -- Initialize BFS with the queue containing the initial path
  where
    {-
        bfs - Auxiliary function that performs bfs to find all shortest paths.

        Parameters:
        - queue: List of paths to be explored (acts as a queue).
        - paths: Accumulated list of shortest paths found so far.
        - visited: Map that stores the known minimum distance for each visited city.

        Returns:
        - A list of shortest paths between 'start' and 'goal'.
    -}
    bfs :: [Path] -> [Path] -> Map.Map City Distance -> [Path]
    bfs [] paths _ = paths  -- If the queue is empty, return the paths found
    bfs (currentPath:rest) paths visited
        | currentCity == goal =
            let currentDist = fromJust (pathDistance roadMap currentPath)  -- Calculate the current path's distance
                minDist = Map.findWithDefault maxBound goal visited  -- Get the known minimum distance for the goal
                visited' = Map.insert goal currentDist visited  -- Update the visited map with the current distance for the goal
                paths' = if currentDist < minDist
                            then [currentPath]  -- Found a new shorter path; replace existing paths
                            else if currentDist == minDist
                                then currentPath : paths  -- Found a path with the same minimum distance; add to the list
                                else paths  -- Longer path, ignore
            in bfs rest paths' visited'  -- Continue BFS with the remaining queue
        | otherwise =
            let neighbors = [neighbor | (neighbor, _) <- adjacent roadMap currentCity]  -- Get neighbors of the current city
                validNeighbors = [neighbor | neighbor <- neighbors, isValidNeighbor neighbor currentPath visited]  -- Filter valid neighbors
                newPaths = [currentPath ++ [neighbor] | neighbor <- validNeighbors]  -- Create new paths by adding valid neighbors
                visited' = foldl updateVisited visited validNeighbors  -- Update the visited map with new distances
            in bfs (rest ++ newPaths) paths visited'  -- Add new paths to the queue and continue BFS
          where
            currentCity = last currentPath  -- Current city is the last city in the current path

            -- Auxiliary function to update the map of visited cities with their minimum distances
            updateVisited :: Map.Map City Distance -> City -> Map.Map City Distance
            updateVisited acc neighbor =
                let newDist = fromJust (pathDistance roadMap (currentPath ++ [neighbor]))  -- Calculate the new distance to the neighbor
                    currentMinDist = Map.findWithDefault maxBound neighbor acc  -- Get the known minimum distance to the neighbor
                in if newDist <= currentMinDist
                    then Map.insert neighbor newDist acc  -- if it's smaller or equal, update 
                    else acc  -- Keep the existing distance if it's smaller

    {-
        isValidNeighbor - Determines if a neighbor should be explored in the next level of the search.

        Parameters:
        - neighbor: Neighboring city to be checked
        - currentPath: Current path traversed up to the current city
        - visited: Map of known minimum distances for visited cities

        Returns:
        - True if the neighbor is not already in the current path (avoids cycles) and the distance to it does not exceed the known minimum
    -}
    isValidNeighbor :: City -> Path -> Map.Map City Distance -> Bool
    isValidNeighbor neighbor currentPath visited =
        notElem neighbor currentPath && newDist <= Map.findWithDefault maxBound neighbor visited
      where
        newPath = currentPath ++ [neighbor]  -- Create a new path including the neighbor
        newDist = fromJust (pathDistance roadMap newPath)  -- Calculate the total distance of the new path

    {-
        fromJust - Extracts the value from a Maybe type, assuming it is Just.

        Parameters:
        - maybeValue: A value of type Maybe a

        Returns:
        - The value contained in Just

    -}
    fromJust :: Maybe a -> a
    fromJust (Just x) = x
    fromJust Nothing = error "Unexpected Nothing value in fromJust"

{-- The travelSales function takes as argument a RoadMap and returns a solution of the Traveling Salesman Problem (TSP), that is the path of the traveling salesman, using a greedy approach.--}
travelSales :: RoadMap -> Path
travelSales roadMap =
    let allCities = cities roadMap
        n = length allCities
        distArray = Data.Array.array ((0, 0), (n - 1, n - 1)) 
                    [((i, j), maybe (maxBound `div` 2) id (distance roadMap (allCities !! i) (allCities !! j))) 
                    | i <- [0..n - 1], j <- [0..n - 1]]
        startCity = 0
        reachableCities = breadthFirstSearch roadMap (allCities !! startCity)
        greedyPath currentCity visitedCities
            | length visitedCities == n = visitedCities ++ [allCities !! startCity]
            | otherwise =
                let nextCity = snd $ minimum [(distArray Data.Array.! (currentCity, j), j) | j <- [0..n - 1], allCities !! j `notElem` visitedCities]
                in greedyPath nextCity (visitedCities ++ [allCities !! nextCity])
    in if length reachableCities == n then greedyPath startCity [allCities !! startCity] else []

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

-- Exemplo de RoadMap para testes
sampleRoadMap :: RoadMap
sampleRoadMap = [
    ("A", "B", 5),
    ("A", "C", 10),
    ("B", "C", 3),
    ("C", "D", 7),
    ("B", "D", 2)
    ]

-- Função principal para testar a shortestPath
main :: IO ()
main = do
    putStrLn "=== Teste da função shortestPath ===\n"

    -- Teste 1: Caminho mais curto entre "A" e "D"
    let path1 = shortestPath sampleRoadMap "A" "D"
    putStrLn "Caminhos mais curtos entre 'A' e 'D':"
    print path1  -- quero [["A","B","D"]]

    -- Teste 2: Caminho mais curto entre "A" e "C"
    let path2 = shortestPath sampleRoadMap "A" "C"
    putStrLn "\nCaminhos mais curtos entre 'A' e 'C':"
    print path2  -- quero [["A","B","C"]]

    -- Teste 3: Caminho mais curto entre "B" e "D"
    let path3 = shortestPath sampleRoadMap "B" "D"
    putStrLn "\nCaminhos mais curtos entre 'B' e 'D':"
    print path3  -- quero [["B","D"]]


    let path4 = shortestPath sampleRoadMap "A" "A"
    putStrLn "\nCaminhos mais curtos entre 'A' e 'A':"
    print path4  -- quero [["A"]]


    let path5 = shortestPath sampleRoadMap "A" "Z"
    putStrLn "\nCaminhos mais curtos entre 'A' e 'Z':"
    print path5  -- quero []