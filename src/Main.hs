module Main where
  import Data.Graph
  import Data.Map (Map)
  import qualified Data.Map as Map


  data Color = Black | White | Red | Blue | Green | Orange | Yellow
  instance Show Color where
    show White = "White"
    show Black = "Black"
    show Red = "Red"
    show Blue = "Blue"
    show Green = "Green"
    show Orange = "Orange"
    show Yellow = "Yellow"
  instance Eq Color where
    c1 == c2 = (show c1) == (show c2)
  data ColorGraph = ColorGraph Graph (Map Int Color)

  arr = [(0, [1, 2, 5]), (1, [0, 3]), (2, [0, 5]), (3, [1, 4]), (4, [3, 5]), (5, [0, 4])]

  main = do
    let colors = Map.unions [ Map.insert x White (Map.fromList [])| x <- [0..5]]
    let graph = buildG (0, 5) $concat [[(a, x)|x<-b]|(a, b)<-arr]
    let colorGraph = (ColorGraph graph colors)
    putStrLn $show $Map.toList colors
    putStrLn $show $checkNeighbors 0 graph
    putStrLn $show $validGraph colorGraph

  checkNeighbors :: Int -> Graph -> [Int]
  checkNeighbors x graph =
    map snd (filter (\xx -> x == fst xx) (edges graph))

  validGraph :: ColorGraph -> Bool
  validGraph (ColorGraph graph colors) =
    length(concat [
      [
        Map.lookup n colors == Map.lookup v colors
          | n <- (checkNeighbors v graph)
      ]
        | v <- (vertices graph)
        ]) == 0



