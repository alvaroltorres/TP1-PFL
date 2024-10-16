import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Text.XHtml (base)

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.
-- The implemented assignment must run under GHC, version 9.10.1.
{-- All code must be properly commented: each function should include
the type declaration (signature) as well a brief description of the function’s goal and the meaning of the arguments.
--}

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


cities :: RoadMap -> [City]
-- queremos retornar a primeira de cada tuplo se não estiver na lista
-- queremos retornar a segunda de cada tuplo se não estiver
-- retornar ambas se nenhuma estiver
-- senão (como ambas estão na lista), não adiciono
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


areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined

distance :: RoadMap -> City -> City -> Maybe Distance
-- quero o tuplo em que essa primeira cidade aparece primeiro
distance [] a b = Nothing
distance ((x, y, z):xs) a b 
    | ((a == x) && b == y) || (b == x && a == y) = Just z
    | otherwise = distance xs a b

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance [] _ = Nothing -- se não existir roadmap
pathDistance _ [] = Nothing -- se não houver cidades
pathDistance _ [x] = Just 0 -- se houver apenas uma cidade no path (destino, final da recursão)
pathDistance roadmap (a:b:xs) = 
    case distance roadmap a b of
        Nothing -> Nothing -- se não existir caminho direto entre a, b, retorno
        Just dist -> case pathDistance roadmap (b:xs) of -- se existir, vou ver se no resto do path existem conexões
                Nothing -> Nothing -- se algures não existir conexão direta, retorno
                Just remainingDist -> Just (dist + remainingDist) -- se tudo correr bem, vou somando os valores dos paths intermediários




rome :: RoadMap -> [City]
rome = undefined

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