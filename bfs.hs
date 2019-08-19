import Test.HUnit
import qualified Data.Map as M
import qualified Data.Set as S

type Vertex = Int
type Graph = M.Map Vertex [Vertex]
type Seen = S.Set Vertex
type Queue = [(Vertex, Int)]

-- This function returns the shortest path in an undirected,
-- unweighted graph from a source to a target vertex by performing
-- a breadth-first-search, and returning the distance from the source
-- to the target once the target is visited. If the target is unreachable
-- from the source, a distance of -1 is returned.
bfs :: Vertex -> Vertex -> Graph -> Int
bfs s t g = bfs_worker [(s, 0)] (S.singleton s) g t

bfs_worker :: Queue -> Seen -> Graph -> Vertex -> Int
bfs_worker [] _ _ _ = -1
bfs_worker ((v, d):vs) seen g t = case v == t of
    True -> d
    False -> case M.lookup v g of
                Nothing -> error "uninitialized edge list for vertex"
                Just neighbors ->
                    let fresh = filter ((flip S.notMember) seen) neighbors in
                    let add_to_queue = map (\n -> (n, d + 1)) fresh in
                    bfs_worker (vs ++ add_to_queue) (S.union (S.fromList fresh) seen) g t

-- Utilities for graph creation
type Edge = (Vertex, Vertex)
type EdgeList = [Edge]

add_dir_edge :: Edge -> Graph -> Graph
add_dir_edge (v1, v2) g = case M.lookup v1 g of
                            Nothing -> M.insert v1 [v2] g
                            Just vs -> M.insert v1 (v2:vs) g

add_undir_edge :: Edge -> Graph -> Graph
add_undir_edge (v1, v2) g = let g_0 = add_dir_edge (v1, v2) g in
                                add_dir_edge (v2, v1) g_0  

mk_graph :: EdgeList -> Graph
mk_graph edges = foldr (\e g -> add_undir_edge e g) M.empty edges

-- Tests
g0 = mk_graph [
                (1,2),(2,3),(3,4),(4,1),(4,5),(1,8),(8,5),(9,10),(11,12),(5,6),
                (6,7),(7,11),(6,8),(11,10),(13,14)
              ]

test_g0 :: Test
test_g0 = TestList 
    [
        -- Simple path test
        "1 3 g0" ~: bfs 1 3 g0 ~?= 2,
        -- Zero distance path test
        "1 1 g0" ~: bfs 1 1 g0 ~?= 0,
        -- Single step test
        "1 2 g0" ~: bfs 1 2 g0 ~?= 1,
        -- Competing paths test
        "4 1 g0" ~: bfs 4 1 g0 ~?= 1,
        -- Longer target test
        "3 11 g0" ~: bfs 3 12 g0 ~?= 6,
        -- Unreachable path test
        "7 13 g0" ~: bfs 1 14 g0 ~?= -1
    ]

g1 = mk_graph [
                (1,2),(2,3),(1,4),(4,3),(3,5),(3,6),(6,5),
                (5,7),(7,8),(8,9),(9,10)
              ]

test_g1 :: Test
test_g1 = TestList
    [
        -- Simple cycle test
        "1 3 g1" ~: bfs 1 3 g1 ~?= 2,
        -- Competing paths test
        "6 4 g1" ~: bfs 6 4 g1 ~?= 2,
        -- Interleaving paths test
        "6 1 g1" ~: bfs 6 1 g1 ~?= 3,
        -- Commutativity test
        "commute 6 1 g1" ~: bfs 6 1 g1 ~?= bfs 1 6 g1,
        -- Random pair test
        "4 1 g1" ~: bfs 4 10 g1 ~?= 6
    ]

main :: IO ()
main = do
   _ <- runTestTT 
            (TestList 
                [
                    test_g0,
                    test_g1
                ])
   return ()

{-|
Answers to follow up questions
1.) When I read the problem, it naturally reduced to a shortest path problem
    in an unweighted, undirected graph (since friendship is transitive
    in this universe). As a result, I knew I needed a graph structure to represent
    the information in the problem, and I used an adjacency list because I wanted
    to perform a bfs, which didn't require indivdual edge existence queries (if it did,
    I may have used an adjacency matrix for O(1) access time for such queries), and
    adjacency lists use less memory too.

2.) The algorithm I used to determine the shortest chain of friends was a breadth
    first search because it is provable that it will find the shortest path between
    two vertices in a graph. In addition, it runs in O(V + E), which, as far as I know,
    is the best that you can do deterministically with random source target pairs in
    every call site (this was clarified in an email). I considered caching intermediate vertices
    for future calls, but I did not do this in this draft solution. I also did not think too much
    about memory requirements as it was clarified in an email that there were no strict memory
    requirements. I can profile the code and can try to reduce any memory bottlenecks if needed.
    
3.) The form in which I provided test cases above was by providing two sample graphs and
    testing the algorithm on different source-target vertex pairs in this graph to ensure correctness. 
    The graphs were crafted to contain cycles, unreachable vertices, multiple paths to target vertices,
    and simple paths to target vertics to test different cases for the bfs implementation. These tests
    do not ensure performance, as the graph is pretty small. If performance tests are needed, I can try
    hacking something up with quickcheck to possibly randomly generate graphs, and then verify shortest
    paths determined by my algorithm with some built in graph library algorithms.
-}