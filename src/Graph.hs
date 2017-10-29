module Graph where
  import Data.Graph
  import Data.Map (Map)
  import Data.Set (Set)
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import Control.Monad (replicateM)
  import Graphviz
  import Color


  -- a color graph is a pair composed of a graph and a map of colors
  data ColorGraph = ColorGraph Graph (Map Int Color) deriving (Show)



  kColorMyGraph given nb_colors = do
    -- Init all the nodes to Yellow
    let colors =
          Map.unions [ Map.insert x Yellow (Map.fromList [])| x <- [1..10]]
    -- building the graph
    let graph =
          buildG (1, 10) $concat [[(a, x)|x<-b]|(a, b)<-given]
    -- building all possible graph, and removing the non valid ones (the ones with 2 neighbors with the same color)
    let kColorsGraphs =
          map (\(ColorGraph a b) -> b)
            $filter validGraph
              $generateAllKColorGraphs graph (length $vertices graph) nb_colors
    -- [Blue Yellow Yellow] == [Yellow Blue Blue] so we remove those duplicates
    let setColors =
          removeDuplicates
            $map (\x -> Set.fromList (map (\(x, y) -> y)
              $Map.toList (reverseMap $Map.toList x))) kColorsGraphs
    let res =
          map
          (Map.toList)
              $map (\x -> setToColors x [Yellow, Blue, Green, Black])
                $map Set.toList setColors

    -- displaying the graphs with graphviz -- work only on kde!!
    sequence [mainGnomes re (edges graph) | re <- take 5 res ]


  setToColors :: [[Int]] -> [Color] -> Map Int Color
  setToColors [] _ =
    Map.fromList []
  setToColors (x:xs) (y:ys) =
    Map.unions (setToColors xs ys :[Map.insert xx y (Map.fromList []) | xx <- x])


  removeDuplicates [] = []
  removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs


  checkNeighbors :: Int -> Graph -> [Int]
  checkNeighbors x graph =
    map snd (filter (\xx -> x == fst xx) (edges graph))

  validGraph :: ColorGraph -> Bool
  validGraph (ColorGraph graph colors) =
    length  (filter (\(x, y) -> Map.lookup x colors == Map.lookup y colors) (edges graph)) == 0


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
        $map (zip [1..i + 1])
          $replicateM i colors

  reverseMap :: [(Int, Color)] -> Map String [Int]
  reverseMap [] = Map.fromList []
  reverseMap ((a, b):xs) = insertReverseMap (reverseMap xs) b a

  insertReverseMap :: Map String [Int] -> Color -> Int -> Map String [Int]
  insertReverseMap map color int =
    case Map.lookup (show color) map of
      Nothing ->  Map.insert (show color) [int] map
      Just n -> Map.insert (show color) (int : n) map


