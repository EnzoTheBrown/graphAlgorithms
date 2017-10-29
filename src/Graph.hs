module Graph where
  import Data.Graph
  import Data.Map (Map)
  import Data.Set (Set)
  import qualified Data.Map as Map
  import qualified Data.Set as Set
  import Control.Monad (replicateM)
  import Graphviz
  import Color
  data ColorGraph = ColorGraph Graph (Map Int Color) deriving (Show)

  kColorMyGraph given nb_colors = do
    let colors =
          Map.unions [ Map.insert x Yellow (Map.fromList [])| x <- [1..10]]
    let graph =
          buildG (1, 10) $concat [[(a, x)|x<-b]|(a, b)<-given]
    let kColorsGraphs =
          map (\(ColorGraph a b) -> b)
            $filter validGraph
              $generateAllKColorGraphs graph (length $vertices graph) nb_colors
    let setColors =
          removeDuplicates
            $map (\x -> Set.fromList (map (\(x, y) -> y)
              $Map.toList (reverseMap $Map.toList x))) kColorsGraphs
    let res =
          map
          (Map.toList)
              $map (\x -> setToColors x [Yellow, Blue, Green, Black])
                $map Set.toList setColors
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


