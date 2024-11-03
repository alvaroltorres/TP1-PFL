import qualified Data.List
import qualified Data.Array
import qualified Data.Bits



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
    let allCitiesList = cities roadMap
        initialCity = case allCitiesList of
            []     -> error "A lista de cidades está vazia"
            (x:_)  -> x
        reachableFromInitial = breadthFirstSearch roadMap initialCity
        reversedRoadMap = [(c2, c1, dist) | (c1, c2, dist) <- roadMap]
        reachableInReversed = breadthFirstSearch reversedRoadMap initialCity
    in length reachableFromInitial == length allCitiesList && length reachableInReversed == length allCitiesList



-- shortestPath function using BFS to find all shortest paths between two cities without using Maps or Sets
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start goal
    | start == goal = [[start]]  -- Se as mesmas cidades, retorna o caminho trivial
    | otherwise     = bfs [[start]] [] []  -- Inicializa o BFS com a fila contendo o caminho inicial e listas vazias
  where
    {-
        bfs - Função auxiliar que realiza o BFS para encontrar todos os caminhos mais curtos.

        Parâmetros:
        - queue: Lista de caminhos a serem explorados (atua como uma fila).
        - paths: Lista acumulada de caminhos mais curtos encontrados até agora.
        - visited: Lista de tuplos (City, Distance) que armazenam a distância mínima conhecida para cada cidade visitada.

        Retorna:
        - Uma lista de caminhos mais curtos entre 'start' e 'goal'.
    -}
    bfs :: [Path] -> [Path] -> [(City, Distance)] -> [Path]
    bfs [] paths _ = paths  -- Se a fila estiver vazia, retorna os caminhos encontrados
    bfs (currentPath:rest) paths visited
        | currentCity == goal =
            let currentDist = fromJust (pathDistance roadMap currentPath)  -- Calcula a distância do caminho atual
                minDist = lookupDistance goal visited  -- Obtém a distância mínima conhecida para o goal
                paths' = if currentDist < minDist
                            then [currentPath]  -- Encontrou um caminho mais curto; substitui os caminhos existentes
                            else if currentDist == minDist
                                then currentPath : paths  -- Encontrou um caminho com a mesma distância mínima; adiciona à lista
                                else paths  -- Caminho mais longo; ignora
                visited' = updateVisitedList currentPath goal currentDist visited  -- Atualiza a lista de visitados com a distância atual para o goal
            in bfs rest paths' visited'  -- Continua o BFS com a fila restante
        | otherwise =
            let neighbors = [neighbor | (neighbor, _) <- adjacent roadMap currentCity]  -- Obtém os vizinhos da cidade atual
                validNeighbors = [neighbor | neighbor <- neighbors, isValidNeighbor neighbor currentPath visited]  -- Filtra vizinhos válidos
                newPaths = [currentPath ++ [neighbor] | neighbor <- validNeighbors]  -- Cria novos caminhos adicionando vizinhos válidos
                visited' = foldl (\acc neighbor ->
                                    let newPath = currentPath ++ [neighbor]
                                        newDist = fromJust (pathDistance roadMap newPath)
                                    in updateVisitedList newPath neighbor newDist acc
                                 ) visited validNeighbors  -- Atualiza a lista de visitados com novas distâncias
            in bfs (rest ++ newPaths) paths visited'  -- Adiciona novos caminhos à fila e continua o BFS
      where
        currentCity = last currentPath  -- Cidade atual é a última cidade no caminho atual

    {-
        updateVisitedList - Atualiza a lista de cidades visitadas com suas distâncias mínimas.

        Parâmetros:
        - currentPath: O caminho atual sendo considerado.
        - city: Cidade a ser atualizada.
        - newDist: Nova distância para a cidade.
        - acc: Lista acumulada de cidades visitadas com suas distâncias.

        Retorna:
        - Lista atualizada de cidades visitadas com suas distâncias.
    -}
    updateVisitedList :: Path -> City -> Distance -> [(City, Distance)] -> [(City, Distance)]
    updateVisitedList currentPath city newDist acc =
        case lookup city acc of
            Nothing -> (city, newDist) : acc  -- Se a cidade não estiver na lista, adiciona-a
            Just existingDist ->
                if newDist <= existingDist
                    then (city, newDist) : filter (\(c, _) -> c /= city) acc  -- Atualiza com a nova distância
                    else acc  -- Mantém a distância existente se ela for menor

    {-
        isValidNeighbor - Determina se um vizinho deve ser explorado no próximo nível da busca.

        Parâmetros:
        - neighbor: Cidade vizinha a ser verificada.
        - currentPath: Caminho atual percorrido até a cidade atual.
        - visited: Lista de distâncias mínimas conhecidas para cidades visitadas.

        Retorna:
        - True se o vizinho não estiver já no caminho atual (evita ciclos) e a distância até ele não exceder a mínima conhecida.
    -}
    isValidNeighbor :: City -> Path -> [(City, Distance)] -> Bool
    isValidNeighbor neighbor currentPath visited =
        notElem neighbor currentPath && newDist <= lookupDistance neighbor visited
      where
        newPath = currentPath ++ [neighbor]  -- Cria um novo caminho incluindo o vizinho
        newDist = fromJust (pathDistance roadMap newPath)  -- Calcula a distância total do novo caminho

    {-
        lookupDistance - Busca a distância mínima conhecida para uma cidade na lista de visitados.

        Parâmetros:
        - city: Cidade cuja distância está a ser buscada.
        - visited: Lista de cidades visitadas com suas distâncias.

        Retorna:
        - A distância mínima conhecida se encontrada, caso contrário, um número muito grande.
    -}
    lookupDistance :: City -> [(City, Distance)] -> Distance
    lookupDistance city [] = maxBound  -- Se não encontrado, retorna um número muito grande
    lookupDistance city ((c, d):xs)
        | c == city  = d
        | otherwise   = lookupDistance city xs

    {-
        fromJust - Extrai o valor de um tipo Maybe, assumindo que é Just.

        Parâmetros:
        - maybeValue: Um valor do tipo Maybe a.

        Retorna:
        - O valor contido em Just.

        Lança:
        - Um erro se chamado com Nothing.
    -}
    fromJust :: Maybe a -> a
    fromJust (Just x) = x
    fromJust Nothing  = error "Unexpected Nothing value in fromJust"

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