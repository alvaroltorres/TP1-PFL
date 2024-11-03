# TP1 PFL

## Graph Representation of a Country

### Project developed by:
    > Rodrigo Miranda up202204916@up.pt
    > Álvaro Torres up202208954@up.pt

## Project collaboration

> Rodrigo Miranda - 50%:
    > Tasks: development of the `cities`, `distance`, `pathDistance`, `shortestPath`, `travelSales`

> Álvaro Torres - 50%:
    > Tasks: development of the `areAdjacent`, `adjacent`, `rome`, `isStronglyConnected`, `travelSales`

## shortestPath Explanation

The `shortestPath` function is designed to find all the shortest paths between two specified cities within a `RoadMap`. It utilizes the Breadth-First Search (BFS) algorithm to ensure that all paths found have the shortest possible distance between the cities.

### Implementation Details

1. **Function Signature**:
   ```haskell
   shortestPath :: RoadMap -> City -> City -> [Path]
   ```
   - Takes a `RoadMap`, a starting city (`start`), and a destination city (`goal`) as input and returns a list of paths, where each path is the shortest route from `start` to `goal`.

2. **Base Case**:
   ```haskell
   shortestPath roadMap start goal
       | start == goal = [[start]]  -- If the same cities, return the trivial path
   ```
   - If the starting city and goal city are the same, it returns a single path containing just that city.

3. **Breadth-First Search (BFS) Initialization**:
   ```haskell
   | otherwise = bfs [[start]] [] Map.empty
   ```
   - The BFS is initialized with a queue containing the initial path (`[start]`). We also keep a list to accumulate found shortest paths (`paths`) and a map (`visited`) to track the minimum known distance to each city.

4. **BFS Process**:
   ```haskell
   bfs :: [Path] -> [Path] -> Map.Map City Distance -> [Path]
   ```
   - The BFS explores each path in the queue, expanding paths by adding valid neighboring cities until it finds the shortest paths to the goal city. It checks each new path against the known minimum distances stored in `visited`.

5. **Updating Paths**:
   - If the current city is the goal city:
     ```haskell
     let currentDist = fromJust (pathDistance roadMap currentPath)
     ```
     - It checks if the path’s distance is shorter than the previously known minimum. If it is, it updates `visited` and stores this path as the new shortest path; otherwise, it adds paths of equal length.

6. **Valid Neighbor Determination**:
   ```haskell
   isValidNeighbor neighbor currentPath visited
   ```
   - The `isValidNeighbor` function checks if a neighboring city should be added to the current path. It ensures that:
     - The neighbor isn’t already in the path (avoiding cycles).
     - The distance to the neighbor doesn’t exceed the minimum distance already recorded in `visited`.

### Auxiliary Functions and Justifications

- **`fromJust`**:
  ```haskell
  fromJust (Just x) = x
  ```
  - A helper function to extract values from `Maybe` types.

- **`visited` Map**:
  - This map efficiently stores the minimum distances to each city, allowing the BFS to focus on paths that could potentially be shorter than those already found.




## travelSales explanation

The `travelSales` function is designed to solve the Traveling Salesman Problem (TSP) using a greedy approach. This function takes a `RoadMap` as an argument and returns the path of the traveling salesman. Below is a detailed explanation of how the function was implemented, including the justification for the auxiliary data structures used and a description of the algorithm.

#### Implementation Details

1. **Extracting All Cities**:
   ```haskell
   let allCities = cities roadMap
       n = length allCities
   ```
   - The function starts by extracting all the cities from the `RoadMap` using the `cities` function. The total number of cities, `n`, is also determined.

2. **Distance Array**:
   ```haskell
   distArray = Data.Array.array ((0, 0), (n - 1, n - 1)) 
               [((i, j), maybe (maxBound `div` 2) id (distance roadMap (allCities !! i) (allCities !! j))) 
               | i <- [0..n - 1], j <- [0..n - 1]]
   ```
   - A 2D array, `distArray`, is created to store the distances between each pair of cities. If there is no direct road between two cities, a large constant value (`maxBound \`div\` 2`) is used to represent the distance. This array allows for efficient lookups of distances during the greedy path construction.

3. **Breadth-First Search for Reachability**:
   ```haskell
   reachableCities = breadthFirstSearch roadMap (allCities !! startCity)
   ```
   - The function performs a breadth-first search (BFS) starting from the first city to determine all reachable cities. This step ensures that the algorithm only attempts to find a TSP path if all cities are reachable.

4. **Greedy Path Construction**:
   ```haskell
   greedyPath currentCity visitedCities
       | length visitedCities == n = visitedCities ++ [allCities !! startCity]
       | otherwise =
           let nextCity = snd $ minimum [(distArray Data.Array.! (currentCity, j), j) | j <- [0..n - 1], allCities !! j `notElem` visitedCities]
           in greedyPath nextCity (visitedCities ++ [allCities !! nextCity])
   ```
   - The `greedyPath` function constructs the TSP path using a greedy approach. Starting from the current city, it selects the nearest unvisited city as the next city to visit. This process continues until all cities have been visited, at which point the path returns to the starting city.

5. **Final Path Determination**:
   ```haskell
   in if length reachableCities == n then greedyPath startCity [allCities !! startCity] else []
   ```
   - The function checks if all cities are reachable. If they are, it calls the `greedyPath` function to construct the TSP path. If not, it returns an empty list, indicating that a valid TSP path does not exist.

#### Justification of Auxiliary Data Structures

- **Distance Array (`distArray`)**:
  - The 2D array is used to store the distances between each pair of cities. This structure allows for efficient O(1) lookups during the greedy path construction, which is crucial for the performance of the algorithm.

- **Breadth-First Search (BFS)**:
  - BFS is used to determine the reachability of all cities from the starting city. This ensures that the algorithm only attempts to find a TSP path if all cities are reachable, preventing unnecessary computations and incorrect results.

#### Algorithm Description

The `travelSales` function uses a greedy algorithm to solve the TSP. The algorithm works as follows:

1. Extract all cities from the `RoadMap` and determine the total number of cities.
2. Create a 2D array to store the distances between each pair of cities.
3. Perform a BFS to determine all reachable cities from the starting city.
4. If all cities are reachable, construct the TSP path using a greedy approach:
   - Start from the first city.
   - At each step, select the nearest unvisited city as the next city to visit.
   - Continue until all cities have been visited, then return to the starting city.
5. If not all cities are reachable, return an empty path.