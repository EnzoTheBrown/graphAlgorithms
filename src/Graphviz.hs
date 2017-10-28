module Graphviz where
  import Color as C
  import System.Process


  colors = ["white", "brown", "blue", "red"]

  getGRaphvizColor :: Color -> String
  getGRaphvizColor x
    | x == Yellow = "yellow"
    | x == Green = "brown"
    | x == Red = "red"
    | x == Black = "black"
    | otherwise = "blue"


  initNodes :: [(Int, Color)] -> String
  initNodes [] = ""
  initNodes ((a, b):xs) =
    "    "
    ++ show a
    ++ "[shape = circle, fillcolor="
    ++ getGRaphvizColor b
    ++ ",style=filled]\n"
    ++ initNodes xs

  initGraph :: [(Int, Int)] -> String
  initGraph [] = ""
  initGraph ((a, b):xs) =
    "    " ++ show a ++ " -> " ++ show b ++" ;\n" ++ initGraph xs


  makeDot colors graph =
     "digraph{\n"
     ++ "    graph [rankdir=LR\n"
     ++ "        bgcolor=transparent];\n"
     ++ "    node [shape=circle\n"
     ++ "         fillcolor=white\n"
     ++ "         style=filled];\n"
     ++ initNodes colors
     ++ initGraph graph
     ++ "}"


  mainGnomes colors graph= do
    writeFile "petersen.dot" $makeDot colors graph
    system "dot -Tpng petersen.dot > petersen.png"
    system "kde-open petersen.png"

