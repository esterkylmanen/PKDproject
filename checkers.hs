
import Control.Monad
import System.Random
import Data.List
import System.IO
import Debug.Trace

data Color a b = Black a b | White a b | Empty a b deriving(Show, Eq)
type Piece = Color Char Int
type Checkerboard = [[Piece]]


startBoard :: Checkerboard
startBoard = [	[(Empty 'A' 1), (Black 'A' 2), (Empty 'A' 3), (Black 'A' 4), (Empty 'A' 5), (Black 'A' 6), (Empty 'A' 7), (Black 'A' 8)], 
				[(Black 'B' 1), (Empty 'B' 2), (Black 'B' 3), (Empty 'B' 4), (Black 'B' 5), (Empty 'B' 6), (Black 'B' 7), (Empty 'B' 8)],
				[(Empty 'C' 1), (Black 'C' 2), (Empty 'C' 3), (Black 'C' 4), (Empty 'C' 5), (Black 'C' 6), (Empty 'C' 7), (Black 'C' 8)], 
				[(Black 'D' 1), (Empty 'D' 2), (Black 'D' 3), (Empty 'D' 4), (Black 'D' 5), (Empty 'D' 6), (Black 'D' 7), (Empty 'D' 8)],
				[(Empty 'E' 1), (Empty 'E' 1), (Empty 'E' 1), (Empty 'E' 1), (Empty 'E' 1), (Empty 'E' 1), (Empty 'E' 1), (Empty 'E' 1)],
				[(Empty 'F' 1), (Empty 'F' 1), (Empty 'F' 1), (Empty 'F' 1), (Empty 'F' 1), (Empty 'F' 1), (Empty 'F' 1), (Empty 'F' 1)],
				[(Empty 'G' 1), (White 'G' 2), (Empty 'G' 3), (White 'G' 4), (Empty 'G' 5), (White 'G' 6), (Empty 'G' 7), (White 'G' 8)], 
				[(White 'H' 1), (Empty 'H' 2), (White 'H' 3), (Empty 'H' 4), (White 'H' 5), (Empty 'H' 6), (White 'H' 7), (Empty 'H' 8)],
				[(Empty 'I' 1), (White 'I' 2), (Empty 'I' 3), (White 'I' 4), (Empty 'I' 5), (White 'I' 6), (Empty 'I' 7), (White 'I' 8)], 
				[(White 'J' 1), (Empty 'J' 2), (White 'J' 3), (Empty 'J' 4), (White 'J' 5), (Empty 'J' 6), (White 'J' 7), (Empty 'J' 8)] ]

-- lägg också till "\n" efter varje lista
showBoard :: Checkerboard -> String
showBoard [] = []
showBoard (x:xs) = showAux x ++ showBoard xs

showAux :: [Piece] -> String
showAux [] = []
showAux ((color char int):xs) 
    | (color char int) == (Black _ _) = "|x" ++ showAux xs
    | (color char int) == (White _ _) = "|o" ++ showAux xs
    | otherwise = "| " ++ showAux xs
  