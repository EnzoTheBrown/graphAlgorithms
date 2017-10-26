module Gnomes where
  import Color as C
  import Data.Text.Lazy (Text, pack, unpack)
  import Data.Graph.Inductive (Gr, mkGraph)
  import Data.GraphViz (
    GraphvizParams,
    GlobalAttributes(
      GraphAttrs,
      NodeAttrs
      ),
    X11Color(Transparent, White),
    nonClusteredParams,
    globalAttributes,
    fmtNode,
    fmtEdge,
    graphToDot
    )
  import Data.GraphViz.Printing (toDot, renderDot)
  import Data.GraphViz.Attributes.Complete (
    DPoint(DVal),
    Attribute(
      Margin,
      Pad,
      Center,
      BgColor,
      FontSize,
      Shape,
      Label,
      ViewPort,
      RankDir,
      Style,
      FillColor
      ),
    Shape(Circle, BoxShape),
    Label(StrLabel),
    ViewPort(VP),
    RankDir(FromLeft),
    StyleName(Filled),
    StyleItem(SItem),
    toWColor,
    wVal,
    hVal,
    zVal,
    focus
    )

  gnomes :: [(Int, C.Color)] -> [(Int, Int)] -> Gr Text Text
  gnomes colors graph = mkGraph (map (\(x, y) -> (x, pack $show y)) colors) (map (\(x, y) -> (x, y, pack "")) graph)

  gnomeParams :: GraphvizParams n Text Text () Text
  gnomeParams = nonClusteredParams {
    globalAttributes = ga,
    fmtNode = fn,
    fmtEdge = fe
    }
    where
      ga = [
        GraphAttrs [
           RankDir FromLeft,
           BgColor [toWColor Transparent]
           ],
        NodeAttrs [
          Shape Circle,
          FillColor [toWColor White],
          Style [SItem Filled []]
          ]
        ]

      fn (n,l) = [(Label . StrLabel) l]
      fe (f,t,l) = [(Label . StrLabel) l]


  mainGnomes colors graph=
    putStr $ unpack $ renderDot $ toDot $ graphToDot gnomeParams (gnomes colors graph)


