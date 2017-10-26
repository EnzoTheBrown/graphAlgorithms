module Main where
  import Data.Graph
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Control.Monad (replicateM)
  import Gnomes
  import Color
  data ColorGraph = ColorGraph Graph (Map Int Color) deriving (Show)

  peterson = [(1, [2, 3, 4]),(2, [1, 7, 8]), (3, [1, 5, 9]),(4, [1, 6, 10]),(5, [3, 6, 8]),(6, [4, 5, 7]),(7, [2, 6, 9]),(8, [2, 4, 10]),(9, [3, 7, 10]),(10, [4, 8, 9])]
  arr = [(0, [1, 2]), (1, [0, 3]), (2, [0, 5]), (3, [1, 4]), (4, [3, 5]), (5, [4])]

  main = do
    let colors = Map.unions [ Map.insert x Yellow (Map.fromList [])| x <- [1..10]]
    let graph = buildG (1, 10) $concat [[(a, x)|x<-b]|(a, b)<-peterson]
    let kColorsGraphs = filter validGraph $generateAllKColorGraphs graph (length $vertices graph) 3
    let (ColorGraph i c) = head kColorsGraphs
    --putStrLn $show $edges i
    --putStrLn $show $Map.toList c
    mainGnomes (Map.toList c) (edges i)

  checkNeighbors :: Int -> Graph -> [Int]
  checkNeighbors x graph =
    map snd (filter (\xx -> x == fst xx) (edges graph))

  validGraph :: ColorGraph -> Bool
  validGraph (ColorGraph graph colors) =
    length(
        concat [filter (1==)[if Map.lookup n colors==Map.lookup v colors
                    then 1
                    else 0
                    |n<-(checkNeighbors v graph)]
                        |v<-(vertices graph)]
                        ) == 0

  generateAllKColorGraphs :: Graph -> Int -> Int -> [ColorGraph]
  generateAllKColorGraphs graph i k =
    generateAllGraphs graph i (
    take k [
            Yellow,
            Black,
            Red,
            Blue,
            Green,
            Orange,
            Yellow
           ]
                              )

  generateAllGraphs :: Graph -> Int -> [Color] -> [ColorGraph]
  generateAllGraphs graph i colors =
    map (ColorGraph graph)
      $map (Map.fromList)
        $map (zip [0..i])
          $replicateM i colors




