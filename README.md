
# TP1 PFL

## Graph Representation of a Country

Este projeto tem como objetivo representar um país como um grafo, onde as cidades são os vértices e as estradas entre elas são as arestas.

### Projeto desenvolvido por:
- **Rodrigo Miranda**: up202204916@up.pt
- **Álvaro Torres**: up202208954@up.pt

### Colaboração no Projeto

Cada membro foi responsável por 50% das tarefas, divididas da seguinte forma:

- **Rodrigo Miranda**:
  - Desenvolvimento das funções `cities`, `distance`, `pathDistance`, `shortestPath` e `travelSales`.
- **Álvaro Torres**:
  - Desenvolvimento das funções `areAdjacent`, `adjacent`, `rome`, `isStronglyConnected` e `travelSales`.

---

## Explicação das Funções Principais

### Função `shortestPath`

A função `shortestPath` usa o algoritmo de Busca em Largura (BFS) para encontrar todos os caminhos mais curtos entre duas cidades, sem o uso de estruturas de dados avançadas como `Map` ou `Set`. Ela toma como argumentos o `RoadMap`, a cidade de origem e a cidade de destino, e devolve uma lista de todos os caminhos mais curtos.

#### Implementação

1. **Casos Base**:
   - Quando a cidade de origem é a mesma que a de destino, retorna `[[start]]` como o caminho trivial.
2. **Busca em Largura (BFS)**:
   - A BFS é implementada para explorar todos os caminhos potenciais, mantendo uma lista dos caminhos mais curtos.
   - Utiliza estruturas auxiliares para armazenar caminhos e distâncias acumuladas, garantindo que apenas os caminhos mais curtos sejam retornados.

### Função `travelSales`

A função `travelSales` é projetada para resolver o Problema do Caixeiro Viajante (TSP) usando uma abordagem gananciosa (greedy). Esta função toma um `RoadMap` como argumento e retorna o caminho do caixeiro viajante.

#### Detalhes da Implementação

1. **Extração das Cidades**:
   - Extrai todas as cidades do `RoadMap` e calcula o número total de cidades `n`.

2. **Array de Distâncias**:
   - Cria uma matriz 2D para armazenar as distâncias entre cada par de cidades. Para pares de cidades sem ligação direta, um valor constante elevado é utilizado.

3. **Busca em Largura para Verificação de Alcance**:
   - Realiza uma BFS a partir da primeira cidade para garantir que todas as cidades são alcançáveis.

4. **Construção do Caminho Ganancioso**:
   - A partir da cidade atual, seleciona a cidade vizinha mais próxima não visitada.
   - Continua até que todas as cidades sejam visitadas e retorna à cidade inicial.

#### Justificação das Estruturas Auxiliares

- **Matriz de Distâncias**: Permite consultas rápidas durante a construção do caminho.
- **BFS**: Garante que o algoritmo só tentará resolver o TSP se todas as cidades forem alcançáveis.

#### Descrição do Algoritmo

O algoritmo usa uma abordagem greedy para construir o caminho do TSP:
1. Seleciona a cidade inicial e constrói o caminho visitando cidades vizinhas próximas não visitadas.
2. Retorna à cidade inicial após visitar todas as cidades.
3. Caso não seja possível alcançar todas as cidades, devolve uma lista vazia.

---


