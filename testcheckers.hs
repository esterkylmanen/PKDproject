
import Control.Monad
--import System.Random
import Data.List
import System.IO
import Debug.Trace

data Color= Black | White | Empty deriving(Show, Eq)
type Piece = (Color, (Char, Int))
type Checkerboard = [[Piece]]

startBoard :: Checkerboard
startBoard = [ [(Empty, ('A', 1)), (Black, ('A', 2)), (Empty, ('A', 3)), (Black, ('A', 4)), (Empty, ('A', 5)), (Black, ('A', 6)), (Empty, ('A', 7)), (Black, ('A', 8))], 
               [(Black, ('B', 1)), (Empty, ('B', 2)), (Black, ('B', 3)), (Empty, ('B', 4)), (Black, ('B', 5)), (Empty, ('B', 6)), (Black, ('B', 7)), (Empty, ('B', 8))],
               [(Empty, ('C', 1)), (Black, ('C', 2)), (Empty, ('C', 3)), (Black, ('C', 4)), (Empty, ('C', 5)), (Black, ('C', 6)), (Empty, ('C', 7)), (Black, ('C', 8))], 
               [(Black, ('D', 1)), (Empty, ('D', 2)), (Black, ('D', 3)), (Empty, ('D', 4)), (Black, ('D', 5)), (Empty, ('D', 6)), (Black, ('D', 7)), (Empty, ('D', 8))],
               [(Empty, ('E', 1)), (Empty, ('E', 2)), (Empty, ('E', 3)), (Empty, ('E', 4)), (Empty, ('E', 5)), (Empty, ('E', 6)), (Empty, ('E', 7)), (Empty, ('E', 8))],
               [(Empty, ('F', 1)), (Empty, ('F', 2)), (Empty, ('F', 3)), (Empty, ('F', 4)), (Empty, ('F', 5)), (Empty, ('F', 6)), (Empty, ('F', 7)), (Empty, ('F', 8))],
               [(Empty, ('G', 1)), (White, ('G', 2)), (Empty, ('G', 3)), (White, ('G', 4)), (Empty, ('G', 5)), (White, ('G', 6)), (Empty, ('G', 7)), (White, ('G', 8))], 
               [(White, ('H', 1)), (Empty, ('H', 2)), (White, ('H', 3)), (Empty, ('H', 4)), (White, ('H', 5)), (Empty, ('H', 6)), (White, ('H', 7)), (Empty, ('H', 8))],
               [(Empty, ('I', 1)), (White, ('I', 2)), (Empty, ('I', 3)), (White, ('I', 4)), (Empty, ('I', 5)), (White, ('I', 6)), (Empty, ('I', 7)), (White, ('I', 8))], 
               [(White, ('J', 1)), (Empty, ('J', 2)), (White, ('J', 3)), (Empty, ('J', 4)), (White, ('J', 5)), (Empty, ('J', 6)), (White, ('J', 7)), (Empty, ('J', 8))] ]
---------------------------------------------------------------------------------------------------------------------------------------------------
drawBoard :: Checkerboard -> IO ()
drawBoard board = putStr ("_________________\n" ++ (boardToString board) ++ "-----------------\n" )


boardToString :: Checkerboard -> String
boardToString [] = []
boardToString (x:xs) = boardToStringAux x ++ boardToString xs

boardToStringAux :: [Piece] -> String
boardToStringAux [] = "|\n"
boardToStringAux ((color ,(_ ,_)):xs)
    | color == Black = "|x" ++ boardToStringAux xs
    | color == White = "|o" ++ boardToStringAux xs
    | otherwise = "|_" ++ boardToStringAux xs

--moves a piece from position one to position two (the players color is required)
--(char1, int1) is the startposition, (char2, int2) is the position to which the piece is moved 
updateBoard :: Checkerboard -> (Char, Int) -> (Char, Int) -> Color -> Checkerboard
updateBoard board (char1, int1) (char2, int2) color = placePiece (removePiece board (char1, int1)) (char2, int2) color

-- finds the piece that is to be moved and removes it from its current position
removePiece :: Checkerboard -> (Char, Int) -> Checkerboard
removePiece [] (char, int) = []
removePiece (x:xs) (char, int) = (removePieceAux x (char, int)):(removePiece xs (char, int))

-- places a piece on the second position with the given color 
placePiece :: Checkerboard -> (Char, Int) -> Color -> Checkerboard
placePiece [] (char, int) color = []
placePiece (x:xs) (char, int) color = (placePieceAux x (char, int) color):(placePiece xs (char, int) color)

--remove the piece from the first position in movePiece
removePieceAux :: [Piece] -> (Char, Int) -> [Piece]
removePieceAux [] (char, int) = []
removePieceAux (x:xs) (char, int) 
    | snd x == (char, int) = (Empty, (char, int)):xs
    | otherwise = x:(removePieceAux xs (char, int))

--place the piece on the second position in movepiece
placePieceAux :: [Piece] -> (Char, Int) -> Color -> [Piece]
placePieceAux [] (char, int) color = []
placePieceAux (x:xs) (char, int) color 
    | snd x == (char,int) = (color,(char, int)):xs
    | otherwise = x:(placePieceAux xs (char, int) color)
---------------------------------------------------------------------------------------------------------------------------------------------------
-- prints piece at (char, int)
getPiece :: Checkerboard -> (Char, Int) -> Piece
getPiece (x:xs) (char, int) = getPieceAux x (char, int)

getPieceAux :: [Piece] -> (Char, Int) -> Piece
getPieceAux (x:xs) (char, int) = (x:xs) !! (int - 1)

-- true if color == piece at (char, int)
isColor :: Checkerboard -> (Char, Int) -> Color -> Bool
isColor (x:xs) (char, int) color = isColorAux x (char, int) color

isColorAux :: [Piece] -> (Char, Int) -> Color -> Bool
isColorAux (x:xs) (char, int) color
     | getPieceAux (x:xs) (char, int) == (color, (char, int)) = True
     | otherwise = False


validMoveBlack :: Checkerboard -> (Char, Int) -> (Char, Int) -> Color -> Bool
validMoveBlack board (char1, int1) (char2, int2) color
    | isColor board (char1, int1) color == True = validMoveBlackAux  board (char1, int1) (char2, int2)
    | otherwise = False 


-- * && (char2 == (char1 - 1)) || (char2 == (char1 + 1))fast det funkar ju inte.. hur ska man göra det??
validMoveBlackAux :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validMoveBlackAux (char1, int1) (char2, int2) 
    | int2 == (int1 + 1) {-- * --} && (isColor board (char2, int2) Empty == True) = True
    | otherwise = False



