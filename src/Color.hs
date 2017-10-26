module Color where

  data Color = Black | Red | Blue | Green | Orange | Yellow
  instance Show Color where
    show Black  = "Black"
    show Red    = "Red"
    show Blue   = "Blue"
    show Green  = "Green"
    show Orange = "Orange"
    show Yellow = "Yellow"
  instance Eq Color where
    c1 == c2 = (show c1) == (show c2)

