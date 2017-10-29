module Main where
  import Graph
  import System.Environment
  import Data.Char

  stringToInt :: String -> Int
  stringToInt [] = 0
  stringToInt x = (digitToInt $last x) + 10*(stringToInt $init x)



  petersen = [
    (1, [2, 5, 6]),
    (2, [1, 3, 7]),
    (3, [2, 4, 8]),
    (4, [3, 5, 9]),
    (5, [4, 1, 10]),
    (6, [1, 8, 9]),
    (7, [2, 9, 10]),
    (8, [3, 6, 10]),
    (9, [4, 6, 7]),
    (10, [5, 7, 8])
             ]
  main = do
    args <- getArgs
    kColorMyGraph petersen $stringToInt $head args

