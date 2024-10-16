import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

{-- All code must be properly commented: each function should include
the type declaration (signature) as well a brief description of the function’s goal and the meaning of the arguments.
--}
{--
The evaluation focuses on
implemented features, the quality and efficiency of the code and respective com
ments, the readme file, and participation in the assignment and presentation.
--}

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities = undefined -- modifiy this line to implement the solution, for each exercise not solved, leave the function definition like this

{-- The areAdjacent function takes as arguments a RoadMap and two Cities 
and returns a Bool indicating whether two cities are linked directly.--}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] _ _ = False
areAdjacent ((a,b,_):xs) c1 c2
    | a == c1 && b == c2 = True
    | a == c2 && b == c1 = True
    | otherwise = areAdjacent xs c1 c2

distance :: RoadMap -> City -> City -> Maybe Distance
distance = undefined

{-- The adjacent function takes as arguments a RoadMap and a City 
and returns a List of (City,Distance) Tuples. The goal of this function is to return the cities adjacent to the given city
and the respective distance between them.--}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((a,b,d):xs) c1
    | a == c1 = (b, d) : adjacent xs c1
    | b == c1 = (a, d) : adjacent xs c1
    | otherwise = adjacent xs c1


pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

{-- The rome function takes as argument a RoadMap and returns 
a List of Cities with the highest number of roads connecting to them.
It uses an auxiliary function cityDegrees that counts the number of connections for each city, taking as arguments a RoadMap and returning a List of (City,Int) Tuples.--}
cityDegrees :: RoadMap -> [(City, Int)]
cityDegrees roadMap = map (\xs -> (head xs, length xs)) (Data.List.group (Data.List.sort cities))
  where cities = concatMap (\(a, b, _) -> [a, b]) roadMap

rome :: RoadMap -> [City]
rome roadMap = map fst (filter (\(_, degree) -> degree == maxDegree) degrees)
  where
    degrees = cityDegrees roadMap
    maxDegree = maximum (map snd degrees)

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]