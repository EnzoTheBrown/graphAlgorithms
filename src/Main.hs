module Main where
  import Data.Graph
  import Data.Map (Map)
  import Data.Set (Set)
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import Control.Monad (replicateM)
  import Gnomes
  import Color
  data ColorGraph = ColorGraph Graph (Map Int Color) deriving (Show)

  peterson = [(1, [2, 3, 4]),(2, [1, 7, 8]), (3, [1, 5, 9]),(4, [1, 6, 10]),(5, [3, 6, 8]),(6, [4, 5, 7]),(7, [2, 6, 9]),(8, [2, 4, 10]),(9, [3, 7, 10]),(10, [4, 8, 9])]
  arr = [(0, [1, 2]), (1, [0, 3]), (2, [0, 5]), (3, [1, 4]), (4, [3, 5]), (5, [4])]

  main = do
    let colors = Map.unions [ Map.insert x Yellow (Map.fromList [])| x <- [1..10]]
    let graph = buildG (1, 10) $concat [[(a, x)|x<-b]|(a, b)<-peterson]
    let kColorsGraphs = map (\(ColorGraph a b) -> b) $filter validGraph $generateAllKColorGraphs graph (length $vertices graph) 3
    let setColors = removeDuplicates $map (\x -> Set.fromList (map (\(x, y) -> y) $Map.toList (reverseMap $Map.toList x))) kColorsGraphs

    putStrLn $show $map (\x -> setToColors x [Yellow, Blue, Green, Black]) $map Set.toList setColors

    --putStrLn $show $Map.toList c
    --mainGnomes (Map.toList c) (edges i)
    -- let d  = Map.fromList [(a, b) | (a, b) <- [Map.toList c]]
    -- putStrLn $show $Map.fromList d
    --putStrLn $show $invert d

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

  reverseMap :: [(Int, Color)] -> Map String [Int]
  reverseMap [] = Map.fromList []
  reverseMap ((a, b):xs) = insertReverseMap (reverseMap xs) b a

  insertReverseMap :: Map String [Int] -> Color -> Int -> Map String [Int]
  insertReverseMap map color int =
    case Map.lookup (show color) map of
      Nothing ->  Map.insert (show color) [int] map
      Just n -> Map.insert (show color) (int : n) map

