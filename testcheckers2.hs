import Control.Monad
import System.Random
import Data.List
import Data.Char
import System.IO
import Debug.Trace
import Test.HUnit

data Color = Black | White | WhiteKing | BlackKing | Empty deriving(Show, Eq)
type Piece = Color
type Checkerboard = [[Color]]

---------------------------------------------------------------------------------------------------------------------------------------------------

{- startBoard
   The starting Checkerboard.
   PRE: True
   RETURNS: a Checkerboard (a list of lists containing Color).
   SIDE EFFECTS: None.
   EXAMPLES: startBoard = [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
               [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
               [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
               [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
               [Empty, White, Empty, White, Empty, White, Empty, White],
               [White, Empty, White, Empty, White, Empty, White, Empty],
               [Empty, White, Empty, White, Empty, White, Empty, White],
               [White, Empty, White, Empty, White, Empty, White, Empty] ]

-}
startBoard :: Checkerboard
startBoard = [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
               [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
               [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
               [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
               [Empty, White, Empty, White, Empty, White, Empty, White],
               [White, Empty, White, Empty, White, Empty, White, Empty],
               [Empty, White, Empty, White, Empty, White, Empty, White],
               [White, Empty, White, Empty, White, Empty, White, Empty] ]


{- rulesString
   Function that provides a string with the rules of the game, print it in terminal by typing: putStr rulesString.
   PRE: True
   RETURNS: A string with the rules of the game
   SIDE EFFECTS: none
   EXAMPLE: rulesString = "\nWelcome to Checkers! \n \nThe rules are simple: \n \n " ++
              "- Pieces can be moved diagonally one step at the time \n " ++
              "- A jump is made if there is a piece of the opposite color diagonally across from a piece of your color\n " ++
              "- If a jump is possible, the jump is mandatory \n " ++
              "- You make the move by giving current position of the piece followed by the position you want to move it to eg. A1 B2 \n"

-}
rulesString :: String
rulesString = "\nWelcome to Checkers! \n \nThe rules are simple: \n \n " ++
              "- Pieces can be moved diagonally one step at the time \n " ++
              "- A jump is made if there is a piece of the opposite color diagonally across from a piece of your color\n " ++
              "- If a jump is possible, the jump is mandatory \n " ++
              "- You make the move by giving current position of the piece followed by the position you want to move it to eg. A1 B2 \n"


{- drawBoard board
   Draws a given board in terminal using I/O action
   PRE: True
   RETURNS: I/O action, printed text in terminal
   SIDE EFFECTS: Draws a 8x8 grid in terminal with added coordinates above and to the left of the grid.
   EXAMPLES:
     drawBoard [[Black]] == "|b|"
     drawBoard [[White]] == "|w|"
     drawBoard [[Empty, Black, Empty, Black, Empty, Black, Empty, Black]] == "   1 2 3 4 5 6 7 8
                                                                              A |_|b|_|b|_|b|_|b|
                                                                              "
     drawBoard startBoard == "   1 2 3 4 5 6 7 8
                              A |_|b|_|b|_|b|_|b|
                              B |b|_|b|_|b|_|b|_|
                              C |_|b|_|b|_|b|_|b|
                              D |b|_|b|_|b|_|b|_|
                              E |_|_|_|_|_|_|_|_|
                              F |_|_|_|_|_|_|_|_|
                              G |_|w|_|w|_|w|_|w|
                              H |w|_|w|_|w|_|w|_|
                              I |_|w|_|w|_|w|_|w|
                              J |w|_|w|_|w|_|w|_|
                              "
-}
drawBoard :: Checkerboard -> IO ()
drawBoard board = putStr ("   1 2 3 4 5 6 7 8 \n" ++ (boardToString board 0 "ABCDEFGHIJ") ++ "\n" )

{- boardToString (x:xs) int list
   Takes a Checkerboard and converts it into a string representing the board
   PRE: True
   RETURNS: A concatinated string of every string from boardToStringAux x
   SIDE EFFECTS: None
   EXAMPLES:
      boardToString [[Black]] 0 "AB" ==  "A |b|\n"
      boardToString [[White]] 0 "AB" ==  "A |w|\n"
      boardToString [[Empty, Black, Empty, Black, Empty, Black, Empty, Black]] 1 "AB" == "B |_|b|_|b|_|b|_|b|\n"
      boardToString [[Empty, Black, Empty, Black, Empty, Black, Empty, Black],[Black, Empty, Black, Empty, Black, Empty, Black, Empty]] 0 "AB" == "A |_|b|_|b|_|b|_|b|\nB |b|_|b|_|b|_|b|_|\n"
-}
boardToString :: Checkerboard -> Int -> String -> String
boardToString [] int list = []
boardToString (x:xs) int list = (list !! int ): " " ++ boardToStringAux x  ++ boardToString xs (int +1) list

{- boardToStringAux (color:xs)
   Converts a list of Color to a string (representing a row of the board)
   PRE: True
   RETURNS: a string based on color in (color:xs) where every different case of color is assosiated with a different string.
   SIDE EFFECTS: None
   EXAMPLES:
      boardToStringAux [Black] == "|b|\n"
      boardToStringAux [White] == "|w|\n"
      boardToStringAux [Empty, Black, Empty, Black, Empty, Black, Empty, Black] == "|_|b|_|b|_|b|_|b|\n"
-}
boardToStringAux :: [Color] -> String
boardToStringAux [] = "|\n"
boardToStringAux (color:xs)
    | color == Black = "|b" ++ boardToStringAux xs
    | color == White = "|w" ++ boardToStringAux xs
    | color == WhiteKing = "|W" ++ boardToStringAux xs
    | color == BlackKing = "|B" ++ boardToStringAux xs
    | otherwise = "|_" ++ boardToStringAux xs

{- exit
   Gives the player a choice between playing agin, and then calling main, or quit playing.
   PRE: True
   SIDE EFFECTS: Prints information in terminal, retrieves information from terminal and analyses it.
                Returns if recieved information == no
   RETURNS: I/O action, prints text in terminal and analyses input from terminal 
-}
exit :: IO ()
exit = do
  putStrLn "rematch? (yes or no)"
  line <- getLine
  if line == "yes" then play
    else return ()

{- main
   Plays the game by calling function playGame with argument startBoard
   PRE: True
   RETURNS: A printed startBoard in terminal
   SIDE EFFECTS: Interaction with game inside function playGame
   EXAMPLE: play =  - Pieces can be moved diagonally one step at the time 
 - A jump is made if there is a piece of the opposite color diagonally across from a piece of your color
 - If a jump is possible, the jump is mandatory 
 - You make the move by giving current position of the piece followed by the position you want to move it to eg. A1 B2 
   1 2 3 4 5 6 7 8 
A |_|b|_|b|_|b|_|b|
B |b|_|b|_|b|_|b|_|
C |_|b|_|b|_|b|_|b|
D |b|_|b|_|b|_|b|_|
E |_|_|_|_|_|_|_|_|
F |_|_|_|_|_|_|_|_|
G |_|w|_|w|_|w|_|w|
H |w|_|w|_|w|_|w|_|
I |_|w|_|w|_|w|_|w|
J |w|_|w|_|w|_|w|_|    -}

play :: IO ()
play = do
  putStrLn rulesString
  playGame startBoard

{- playGame
   Controls whos turn it is to play and keeps track if someone wins.
   PRE: True
   RETURNS:
   SIDE EFFECTS: Interaction with game inside function playGame and prints information i terminal
   EXAMPLES : playGame startBoard 
   1 2 3 4 5 6 7 8 
A |_|b|_|b|_|b|_|b|
B |b|_|b|_|b|_|b|_|
C |_|b|_|b|_|b|_|b|
D |b|_|b|_|b|_|b|_|
E |_|_|_|_|_|_|_|_|
F |_|_|_|_|_|_|_|_|
G |_|w|_|w|_|w|_|w|
H |w|_|w|_|w|_|w|_|
I |_|w|_|w|_|w|_|w|
J |w|_|w|_|w|_|w|_|

Your move:

-}
playGame :: Checkerboard -> IO ()
playGame board = do
  drawBoard board
  putStrLn "Your move:"
  newboard <- playersMove board
  if win newboard White then do
    putStrLn "You won!\n"
    exit
    else do
      if (kingCheck newboard White /= []) then do
        newestboardA <- computerMove (placeKing newboard (kingCheck newboard White) White)
        if win newestboardA Black then do
          putStrLn "Computer won!\n"
          exit
          else do
            if (kingCheck newestboardA Black /= []) then do
              playGame (placeKing newestboardA (kingCheck newestboardA Black) Black)
              else do playGame newestboardA
          else do
            newestboardB <- computerMove newboard
            if win newestboardB Black then do
              putStrLn "Computer won!\n"
              exit
              else do
                if (kingCheck newestboardB Black /= []) then do
                  playGame (placeKing newestboardB (kingCheck newestboardB Black) Black)
                  else do playGame newestboardB



{- printMove string (char1, int1) (char2, int2)
   PRE: True
   RETURNS: IO action
   SIDE EFFECTS: Prints given string and positions in terminal
   EXAMPLES: printMove "player" ('A', 1) ('B', 2) == player made move from A1 to B2
-}
printMove :: String -> (Char, Int) -> (Char, Int) -> IO ()
printMove string (char1, int1) (char2, int2) =
  putStr $ string ++ " made move from " ++ char1:show int1 ++ " to " ++ char2:show int2 ++ "\n"

{- playersMove board
   Retreives and analyses the players move from terminal
   PRE: True
   RETURNS: a checkerboard of type IO Checkerboard, text in terminal
   SIDE EFFECTS: Retrieves the input, that is the move, from terminal and prints info on terminal about the input
   EXAMPLES:> playersMove startBoard 
            > GG
            > Invalid move, try again! Make move by typing it in form A1 B2 
-}
playersMove :: Checkerboard -> IO Checkerboard
playersMove board = do
    if jumpMoveAvailableWhite board then do
      putStrLn "Jump is mandatory!"
      move <- getLine
      if length move > 4 && validInput move && validJumpWhite board (handleMove move) (handleMove (drop 3 move)) then do
        printMove "You" (handleMove move) (handleMove (drop 3 move))
        return $ (updateBoard (removeJumpedPiece board (handleMove move) (handleMove (drop 3 move))) (handleMove move) (handleMove (drop 3 move)))
        else do
          if length move > 4 && (getPiece board (handleMove move) == WhiteKing) && validJumpWhiteKing board (handleMove move) (handleMove (drop 3 move))
            then do
              printMove "You" (handleMove move) (handleMove (drop 3 move))
              return $ (updateBoard (removeJumpedPiece board (handleMove move) (handleMove (drop 3 move))) (handleMove move) (handleMove (drop 3 move)))
              else do
                putStrLn "Invalid move, try again! Make move by typing it in form A1 B2 \n"
                playersMove board
      else do
        move <- getLine
        if length move > 4 && validInput move && validMove board (handleMove move) (handleMove (drop 3 move)) White then do
          printMove "You" (handleMove move) (handleMove (drop 3 move))
          return $ (updateBoard board (handleMove move) (handleMove (drop 3 move)))
          else do
            if length move > 4 && (getPiece board (handleMove move) == WhiteKing) && validMoveKing board (handleMove move) (handleMove (drop 3 move))
              then do
                printMove "You" (handleMove move) (handleMove (drop 3 move))
                return $ (updateBoard board (handleMove move) (handleMove (drop 3 move)))
                else do
                  putStrLn "Invalid move, try again! Make move by typing it in form A1 B2 \n"
                  playersMove board


{- removeJumpedPiece board (char1, int1) (char2, int2)
   Removes the pice that is jumped over from the bord.
   PRE: The jump is expected to be a valid move
   RETURNS: board without the piece in between (char1, int1) and (char2, int2).
   SIDE EFFECTS: None
   EXAMPLES:
      removeJumpedPiece testBoard ('F', 3) ('D', 1) == [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                         [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                         [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                         [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                         [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                         [Empty, Empty, White, Empty, Empty, Empty, Empty, Empty],
                                                         [Empty, White, Empty, Empty, Empty, White, Empty, White],
                                                         [White, Empty, White, Empty, White, Empty, White, Empty],
                                                         [Empty, White, Empty, White, Empty, White, Empty, White],
                                                         [White, Empty, White, Empty, White, Empty, White, Empty] ]
       removeJumpedPiecetestBoard ('E', 2) ('G', 4) == [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                          [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                          [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                          [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                          [Empty, Black, Empty, Empty, Empty, Empty, Empty, Empty],
                                                          [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                          [Empty, White, Empty, Empty, Empty, White, Empty, White],
                                                          [White, Empty, White, Empty, White, Empty, White, Empty],
                                                          [Empty, White, Empty, White, Empty, White, Empty, White],
                                                          [White, Empty, White, Empty, White, Empty, White, Empty] ]
-}
removeJumpedPiece :: Checkerboard -> (Char, Int) -> (Char, Int) -> Checkerboard
removeJumpedPiece [] (char1, int1) (char2, int2) = []
removeJumpedPiece board (char1, int1) (char2, int2)
    | char2 > char1 && int2 > int1 = removePiece board ((nextChar char1), (int1 + 1))
    | char2 > char1 && int2 < int1 = removePiece board ((nextChar char1), (int1 - 1))
    | char2 < char1 && int2 > int1 = removePiece board ((prevChar char1), (int1 + 1))
    | char2 < char1 && int2 < int1 = removePiece board ((prevChar char1), (int1 - 1))

{- handleMove input
   Converst input coordinates for a move (a sting eg. "A1") to a tuple based on the coordinates.
   PRE: True
   RETURNS: input as (x, y) where x is the first character in input and y the Int assosiated with the second character in input.
   SIDE EFFECTS: None.
   EXAMPLES:
      handleMove "A1" == ('A', 1)
      handleMove "J8" == ('J', 8)
-}
handleMove :: String -> (Char, Int)
handleMove input = ((input !! 0), digitToInt (input !! 1))

{- validInput string
   Varifies that the second character of the input coordinates are valid (that it is a number 1-8).
   PRE: None
   RETURNS: True if both the second and the forth character of string have ord between 48 and 57. Otherwise False.
   SIDE EFFECTS: None.
   EXAMPLES:
      validInput "A1 B2" == True
      validInput "H4 D7" == True
      validInput "J8 I9" == False
      validInput "B 4C5" == False
-}
validInput :: String -> Bool
validInput string = ((ord (string !! 1)) > 48 && (ord (string !! 1)) < 57) && ((ord (string !! 4)) > 48 && (ord (string !! 4)) < 57)
--------------------------------------------------------------------------------

{- computerMove board
   Calculates the computers move
   PRE: True
   RETURNS: a checkerboard of type IO Checkerboard, text in terminal
   SIDE EFFECTS: Prints information in terminal and draws the recieved board in terminal
   EXAMPLES: computerMove startBoard 
   1 2 3 4 5 6 7 8 
A |_|b|_|b|_|b|_|b|
B |b|_|b|_|b|_|b|_|
C |_|b|_|b|_|b|_|b|
D |b|_|b|_|b|_|b|_|
E |_|_|_|_|_|_|_|_|
F |_|_|_|_|_|_|_|_|
G |_|w|_|w|_|w|_|w|
H |w|_|w|_|w|_|w|_|
I |_|w|_|w|_|w|_|w|
J |w|_|w|_|w|_|w|_|

Computer made move from D7 to E6
[[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
[Black,Empty,Black,Empty,Black,Empty,Black,Empty],
[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
[Black,Empty,Black,Empty,Black,Empty,Empty,Empty],
[Empty,Empty,Empty,Empty,Empty,Black,Empty,Empty],
[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
[Empty,White,Empty,White,Empty,White,Empty,White],
[White,Empty,White,Empty,White,Empty,White,Empty],
[Empty,White,Empty,White,Empty,White,Empty,White],
[White,Empty,White,Empty,White,Empty,White,Empty]]
-}

computerMove :: Checkerboard -> IO Checkerboard
computerMove board = do
    drawBoard board
    if jumpMoveAvailableBlack board  then do computerMoveJump board
      else do computerMoveDiagonal board

{- computerMoveJump board
   Calculates a computermove if a jump is possible
   PRE: True
   RETURNS: a checkerboard of type IO Checkerboard
   SIDE EFFECTS: in called functions
   EXAMPLES: computerMoveJump startBoard
Computer made move from D5 to F7
[[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
[Black,Empty,Black,Empty,Black,Empty,Black,Empty],
[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
[Black,Empty,Black,Empty,Empty,Empty,Black,Empty],
[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
[Empty,Empty,Empty,Empty,Empty,Empty,Black,Empty],
[Empty,White,Empty,White,Empty,White,Empty,White],
[White,Empty,White,Empty,White,Empty,White,Empty],
[Empty,White,Empty,White,Empty,White,Empty,White],
[White,Empty,White,Empty,White,Empty,White,Empty]]

note that startboar in example above i modified to create a sitiuation where a jump is possible
-}
computerMoveJump :: Checkerboard -> IO Checkerboard
computerMoveJump board = getRandomJumpMove board (allJumpMovesBlack board ++ allJumpMovesBlackKing board)

{- computerMoveDiagonal board
   Calculates a computermove if a jump is not possible
   PRE: True
   RETURNS: a checkerboard of type IO Checkerboard
   SIDE EFFECTS: none
   EXAMPLES: computerMoveDiagonal startBoard 
Computer made move from D5 to E4
[[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
[Black,Empty,Black,Empty,Black,Empty,Black,Empty],
[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
[Black,Empty,Black,Empty,Empty,Empty,Black,Empty],
[Empty,Empty,Empty,Black,Empty,White,Empty,Empty],
[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
[Empty,White,Empty,White,Empty,White,Empty,White],
[White,Empty,White,Empty,White,Empty,White,Empty],
[Empty,White,Empty,White,Empty,White,Empty,White],
[White,Empty,White,Empty,White,Empty,White,Empty]]
-}
computerMoveDiagonal :: Checkerboard -> IO Checkerboard
computerMoveDiagonal board = getRandomDiagonalMove board (allDiagonalMovesBlack board)

{- getRandomJumpMove board list
   Pics a random element from the given list
   PRE: True
   RETURNS: a checkerboard of type IO Checkerboard
   SIDE EFFECTS: retrieves a random numer using randomRIO
   -}

getRandomJumpMove :: Checkerboard -> [(Char, Int)] -> IO Checkerboard
getRandomJumpMove board list = do
          int <- (randomRIO (0, (length list - 1)))
          printMove "Computer" (localizeJump board (pickMove board list int)) (pickMove board list int)
          return (updateBoard (removeJumpedPiece board (localizeJump board (pickMove board list int)) (pickMove board list int)) (localizeJump board (pickMove board list int)) (pickMove board list int))


{- getRandomDiagonalMove board list
   Pics a random element from the given list
   PRE: True
   RETURNS: a checkerboard of type IO Checkerboard
   SIDE EFFECTS: retrieves a random numer using randomRIO
   -}
getRandomDiagonalMove :: Checkerboard -> [(Char, Int)] -> IO Checkerboard
getRandomDiagonalMove board list = do
          int <- (randomRIO(0, (length list - 1)))
          printMove "Computer" (localizeDiagonal board (pickMove board list int)) (pickMove board list int)
          return (updateBoard board (localizeDiagonal board (pickMove board list int)) (pickMove board list int))

{- pickMove
   Picks out the move on the position of int from a list of moves.
   PRE: True.
   RETURNS: the move that is on the position of int in list.
   SIDE EFFECTS: None.
   EXAMPLES:
      pickMove startBoard [('A',2), ('B',3)] 0 == ('A',2)
      pickMove startBoard [('A',2), ('B',3)] 1 == ('B',3)
-}
pickMove :: Checkerboard -> [(Char, Int)] -> Int -> (Char, Int)
pickMove board list int = list !! int

{- localizeJump board (char, int)
   Localizes a black jump.
   PRE: True.
   RETURNS: the position (char, int) if it is a possible black jump.
   SIDE EFFECTS: None.
   EXAMPLES:
      localizeJump startBoard ('F', 1) == ('F', 1)
      localizeJump startBoard ('F', 7) == ('F', 7)
-}
localizeJump :: Checkerboard -> (Char, Int) -> (Char, Int)
localizeJump board (char, int)
    | (blackTwoBackWhiteOneBackLeft board (char, int)) = ((prevChar (prevChar char)), (int + 2))
    | (blackTwoBackWhiteOneBackRight board (char, int)) = ((prevChar (prevChar char)), (int - 2))

{- localizeDiagonal board (char, int)
   Localizes a diagonal black move.
   PRE: True.
   RETURNS: the position (char, int) if it is a possible diagonal black move.
   SIDE EFFECTS: None.
   EXAMPLES:
      localizeDiagonal startBoard ('E', 2) == ('E', 2)
      localizeDiagonal startBoard ('E', 8) == ('E', 8)
-}
localizeDiagonal :: Checkerboard -> (Char, Int) -> (Char, Int)
localizeDiagonal board (char, int)
    | ((isColor board (((prevChar char)), (int + 1)) Black) || (isColor board (((prevChar char)), (int + 1)) BlackKing)) = ((prevChar char), (int + 1))
    | ((isColor board (((prevChar char)), (int - 1)) Black) || (isColor board (((prevChar char)), (int - 1)) BlackKing)) = ((prevChar char), (int - 1))


{- blackTwoBackWhiteOneBackLeft board (char, int)
   helpes to calculate the computermove by checking the surroundings of the given position
   PRE: (char, int) must be a valid position i given board
   RETURNS: True if surrounding is correkt otherwise false
   SIDE EFFECTS: None.
-}

blackTwoBackWhiteOneBackLeft :: Checkerboard -> (Char, Int) -> Bool
blackTwoBackWhiteOneBackLeft board (char, int)
    | ((isColor board ((prevChar (prevChar char)), (int + 2)) Black) || (isColor board ((prevChar (prevChar char)), (int + 2)) BlackKing)) && ((isColor board ((prevChar char), (int + 1)) White) || (isColor board ((prevChar char), (int + 1)) WhiteKing)) = True
    | otherwise = False

{- blackTwoBackWhiteOneBackRight board (char, int)
   helpes to calculate the computermove by checking the surroundings of the given position, similar as above
   PRE: (char, int) must be a valid position i given board
   RETURNS: True if surrounding is correkt otherwise false
   SIDE EFFECTS: None.
-}
blackTwoBackWhiteOneBackRight :: Checkerboard -> (Char, Int) -> Bool
blackTwoBackWhiteOneBackRight board (char, int)
    | ((isColor board ((prevChar (prevChar char)), (int - 2)) Black) || (isColor board ((prevChar (prevChar char)), (int - 2)) BlackKing)) && ((isColor board ((prevChar char), (int - 1)) White) || (isColor board ((prevChar char), (int - 1)) WhiteKing)) = True
    | otherwise = False
--------------------------------------------------------------------------------
-- MOVE PIECES


{- updateBoard board (char1, int1) (char2, int2)
   Moves a piece of datatype Color from one position (char1, int1) to another (char2, int2)
   PRE: True
   RETURNS: an updated board with the piece at position (char1, int1) moved to position (char2, int2)
   SIDE EFFECTS: None
   EXAMPLES:  updateBoard startBoard ('D',1) ('E',2) Black == [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                [Empty, Black, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                [White, Empty, White, Empty, White, Empty, White, Empty],
                                                                [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                [White, Empty, White, Empty, White, Empty, White, Empty] ]
-}
updateBoard :: Checkerboard -> (Char, Int) -> (Char, Int) -> Checkerboard
updateBoard board (char1, int1) (char2, int2) = removePiece (placePiece board (char1, int1) (char2, int2)) (char1, int1)

{- removePiece board (char, int)
   Removes the piece from the given position in the given checkerboard
   PRE: board must be of type Checkerboard and can not be empty
   RETURNS: The given Checkerboard but without the piece in given position
   SIDE EFFECTS: none
   EXAMPLES: removePiece [[Empty,Black,Empty]] ('A', 2) == [[Empty,Empty,Empty]]
-}
removePiece :: Checkerboard -> (Char, Int) -> Checkerboard
removePiece board (char, int) = take (ord char - 65) board ++ (removePieceAux (board !! (ord char - 65)) int ):(drop (ord char - 64) board)

{- removePieceAux colorlist int
   removes the piece from a list of colors/pieces on the given position
   PRE: int<= (length colorlist) && int > 0
   RETURNS: the same list of colors as the argument but without the piece in position nr int
   SIDE EFFECTS: none
   EXAMPLES: removePieceAux [Black] 1 == [Empty]
-}
removePieceAux :: [Color] -> Int -> [Color]
removePieceAux colorlist int = take (int - 1) colorlist ++ (Empty:(drop int colorlist))

{- placePiece board (char1, int1) (char2, int2)
   Places the same piece that is positioned on (char1, int1) on the the second position (char2, int2)
   PRE: nonempty board
   RETURNS: the argument board but with an added piece on position (char2, int2)
   SIDE EFFECTS: none
   EXAMPLES:  placePiece [[Black, Empty, Black]] ('A', 1) ('A', 2) == [[Black,Black,Black]]
-}
placePiece :: Checkerboard -> (Char, Int) -> (Char, Int) -> Checkerboard
placePiece board (char1, int1) (char2, int2) = take (ord char2 - 65) board ++ (placePieceAux (board !! (ord char2 - 65)) int2 (getPiece board (char1, int1))):(drop (ord char2 - 64) board)

{- placePieceAux colorlist int color
   Adds a piece with the same color as the argument color on given position i a list of colors
   PRE: none
   RETURNS: the list colorlist with a piece with the same color as the argument "color" on positon int
   SIDE EFFECTS: none
   EXAMPLES: placePieceAux [Empty] 1 Black == [Black]
   placePieceAux [Black, Empty, Black, Empty] 2 Black == [Black,Black,Black,Empty]
-}
placePieceAux :: [Color] -> Int -> Color -> [Color]
placePieceAux colorlist int color = take (int - 1) colorlist ++ (color:(drop int colorlist))

{- getPiece board (char, int)
   Get the color of the piece at cooridinates (char, int) in board.
   PRE:  (char, int) are coordinates that exists in board.
   RETURNS: the Color of the position (char, int) in board.
   SIDE EFFECTS: None.
   EXAMPLES:  getPiece startBoard ('A', 1) == Empty
              getPiece startBoard ('B', 1) == Black
-}
getPiece :: Checkerboard -> (Char, Int) -> Color
getPiece board (char, int) = ((board !! (ord char - 65)) !! (int - 1))

{- isColor board (char, int) color
   Checks if the color of the piece at the coordinated (char, int) in the Checkboard are the color as Color
   PRE: (char, int) are coordinates that exists in the board Checkerboard
        color is a color in the datatype Color
   RETURNS: True if color == color of the piece at (char, int) in Checkerboard
            False if color != color of the piece at (char, int) in Checkerboard
   SIDE EFFECTS: False
   EXAMPLES:  isColor startBoard ('A', 1) Black == False
              isColor startBoard ('A', 1) Empty == True
-}
isColor :: Checkerboard -> (Char, Int) -> Color -> Bool
isColor board (char, int) color
    | (validPosition (char, int) == False) = False
    | otherwise = getPiece board (char, int) == color

{- aMove (char1, int1) (char2, int2)
   Controls that the position of the pice is not the same as the attemted move.
   PRE: True.
   RETURNS: False if char1 = char2 and int1 = int2, otherwise True.
   SIDE EFFECTS: None.
   EXAMPLES:
      aMove ('A', 1) ('A', 1) == False
      aMove ('A', 1) ('B', 2) == True
      aMove ('A', 1) ('B', 1) == True
-}
aMove :: (Char, Int) -> (Char, Int) -> Bool
aMove (char1, int1) (char2, int2)
    | ((ord char1 - 64) == (ord char2 -64)) && (int1 == int2) = False
    | otherwise = True

{- validPosition (char, int)
   Determines whether the position is a valid position on Checkerboard.
   PRE: True.
   RETURNS: True if char is a character of ord 65-74 (A-J) and int is a character of ord 1-8 (number 1-8).
   SIDE EFFECTS: None.
   EXAMPLES:
      validPosition ('A', 1) == True
      validPosition ('J', 8) == True
      validPosition ('a', 1) == False
      validPosition ('J', 9) == False
-}
validPosition :: (Char, Int) -> Bool
validPosition (char, int) = ((ord char >= 65) && (ord char <= 74)) && ((int >= 1) && (int <= 8))

{- validValue board (char1, int1) (char2, int2)
   Makes sure that char1 and char2 is characters between A-J with captital letters and that int1 and int2 is between 1-8
   PRE: Arguments must be in correct form
   RETURNS: True if arguments an in correct value range
   SIDE EFFECTS: None
   EXAMPLES:
     validValue startBoard ('A', 1) ('A',1) == True
     validValue startBoard ('A', 1) ('A',9) == False
     validValue startBoard ('A', 1) ('K',1) == False
-}
validValue :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validValue board (char1, int1) (char2, int2) =
    (((ord char1 >= 65) && (ord char1 <= 74)) && (ord char2 >= 65)) && ((ord char2 <= 74) && ((int1 >= 1) && ((int1 <= 8) && ((int2 >= 1) && (int2 <= 8)))))

{- validMove board (char1, int1) (char2, int2) color
   Checks if a move with a piece with that color from the coordinates (char1, int1) to the coordinates (char2, int2) the board is valid
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in the board
          color is in the datatype Color
   RETURNS: True if a move with a piece of Color from the coordinates (char1, int1) to the coordinates (char2, int2) in the board is valid
            False if a move with a piece of Color from the coordinates (char1, int1) to the coordinates (char2, int2) in the board is not valid
            False if the coorinate (char2, int2) is Empty
   SIDE EFFECTS: None
   EXAMPLES:  validMove startBoard ('D', 1) ('E', 2) Black == True
              validMove startBoard ('D', 1) ('E', 1) Black == False
              validMove startBoard ('D', 2) ('E', 2) White == False
-}
validMove :: Checkerboard -> (Char, Int) -> (Char, Int) -> Color -> Bool
validMove board (char1, int1) (char2, int2) color
    | (validValue board (char1, int1) (char2,int2) == False) || (aMove (char1, int1) (char2, int2) == False) = False
    | (color == White) && (isColor board (char1, int1) White) = validMoveWhite board (char1, int1) (char2, int2)
    | (color == Black) && (isColor board (char1, int1) Black)= validMoveBlack board (char1, int1) (char2, int2)
    | (color == BlackKing) && (isColor board (char1, int1) BlackKing) = validMoveKing board (char1, int1) (char2, int2)
    | (color == WhiteKing) && (isColor board (char1, int1) WhiteKing) = validMoveKing board (char1, int1) (char2, int2)
    | otherwise = False


{- validMoveBlack board (char1, int1) (char2, int2)
   Checks if a move with a black piece from the coordinates (char1, int1) to the coordinates (char2, int2) in the board is valid
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in the board
          There is a black piece at coorinates (char1, int1) in board Checkboard
   RETURNS: True if a move with a black piece from the coordinates (Char, Int) to the coordinates (Char, Int) in board Checkboard is valid
            False if a move with a black piece from the coordinates (Char, Int) to the coordinates (Char, Int) in board Checkboard is not valid
            False if the second (Char, Int) (to where you want to move a piece) is occupied
   SIDE EFFECTS: False
   EXAMPLES:  validMove startBoard ('D', 1) ('E', 2) = True
              validMove startBoard ('D', 1) ('E', 1) = False
-}
validMoveBlack :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validMoveBlack board (char1, int1) (char2, int2)
    | (isSquareEmpty board (char2, int2)) && (correctDiagonalBlack board (char1, int1) (char2, int2)) = True
    | otherwise = False

{- validMoveWhite Checkerboard (Char, Int) (Char, Int)
   Checks if a move with a white piece from the coordinates (Char, Int) to the coordinates (Char, Int) in board Checkboard is valid
   PRE:   (Char, Int) and (Char, Int) are coordinates that exists in the board Checkerboard
          There is a white piece at coorinates (Char, Int) in board Checkboard
   RETURNS: True if a move with a white piece from the coordinates (Char, Int) to the coordinates (Char, Int) in board Checkboard is valid
            False if a move with a white piece from the coordinates (Char, Int) to the coordinates (Char, Int) in board Checkboard is not valid
            False if the second coordinate (Char, Int) (to where you want to move a piece) is occupied
   SIDE EFFECTS: False
   EXAMPLES:  validMove startBoard ('G', 2) ('F', 1) = True
              validMove startBoard ('G', 2) ('F', 2) = False
-}
validMoveWhite :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validMoveWhite board (char1, int1) (char2, int2)
    | (isSquareEmpty board (char2, int2)) && (correctDiagonalWhite board (char1, int1) (char2, int2)) = True
    | otherwise = False

{- validMoveKing board (char1, int1) (char2, int2)
   Checks if a move for either a black or white pice is valid .
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in board.
          There is a piece at coorinates (char1, int1) in board
   RETURNS: True if a move with a white or black piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board is valid.
            Otherwise False.
   SIDE EFFECTS: None.
   EXAMPLES:  validMoveKing kingTestBoard ('A', 6) ('B', 7) == True
              validMoveKing kingTestBoard ('I', 4) ('H', 3) == True
              validMoveKing kingTestBoard ('I', 4) ('h', 3) == False
              validMoveKing kingTestBoard ('A', 6) ('B', 8) == False
-}
validMoveKing :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validMoveKing board (char1, int1) (char2, int2)
    | (validMoveBlack board (char1, int1) (char2, int2)) = True
    | (validMoveWhite board (char1, int1) (char2, int2)) = True
    | otherwise = False

{- correctDiagonalBlack board (char1, int1) (char2, int2)
   Checks if a regular, diagonal, move with a black piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board Checkboard is valid
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in the board Checkerboard
          There is a black piece at coorinates (char1, int1) in board.
          The second coordinate (char2, int2) (to where you want to move a piece) is Empty
   RETURNS: True if a regular, diagonal, move with a black piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board is valid.
            Otherwise false.
   SIDE EFFECTS: None.
   EXAMPLES:  validMove startBoard ('D', 1) ('E', 2) == True
              validMove startBoard ('D', 1) ('E', 1) == False
-}
correctDiagonalBlack :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
correctDiagonalBlack board (char1, int1) (char2, int2)
    | (int2 == (int1 - 1) || int2 == (int1 + 1)) && ((ord char2 - 64) == ((ord char1 - 64) + 1)) = True
    | otherwise = False

{- correctDiagonalWhite board (char1, int1) (char2, int2)
   Checks if a regular, diagonal, move with a white piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board is valid.
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in board.
          There is a white piece at coorinates (char1, int1) in board.
          The second coordinate (char2, int2) (to where you want to move a piece) is Empty
   RETURNS: True if a regular, diagonal, move with a white piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board is valid.
            Otherwise False.
   SIDE EFFECTS: None.
   EXAMPLES:  validMove startBoard ('G', 2) ('F', 1) == True
              validMove startBoard ('G', 2) ('F', 2) ==  False
-}
correctDiagonalWhite :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
correctDiagonalWhite board (char1, int1) (char2, int2)
    | (int2 == (int1 - 1) || int2 == (int1 + 1)) && ((ord char2 - 64) == ((ord char1 - 64) - 1)) = True
    | otherwise = False

{- validJumpBlack board (char1, int1) (char2, int2)
   Checks if a move where you jump over a white piece with a black piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board is valid.
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in board.
          There is a black piece at coorinates (char1, int1) in board Checkboard
          The second coordinate (char2, int2) (to where you want to move a piece) is Empty
   RETURNS: True if the square to jump over is a white piece
            False if the square to jump over is not a white piece
   SIDE EFFECTS: None.
   EXAMPLES:  validMove updatedBoard ('D', 1) ('F', 3) == True
              validMove startBoard ('D', 1) ('F', 3) == False
-}
validJumpBlack :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validJumpBlack board (char1, int1) (char2, int2)
    | (validValue board (char1, int1) (char2, int2) == False) = False
    | (getPiece board (char1, int1) == WhiteKing) && ((isSquareEmpty board (char2, int2)) && (int2 < int1) && ((isColor board ((nextChar char1), (int2 + 1)) Black) || (isColor board ((nextChar char1), (int2 + 1)) BlackKing))) = True
    | (getPiece board (char1, int1) == WhiteKing) && ((isSquareEmpty board (char2, int2)) && (int2 > int1) && ((isColor board ((nextChar char1), (int2 - 1)) Black) || (isColor board ((nextChar char1), (int2 - 1)) BlackKing))) = True
    | (isSquareEmpty board (char2, int2)) && (int2 < int1) && ((isColor board ((nextChar char1), (int2 + 1)) White) || (isColor board ((nextChar char1), (int2 + 1)) WhiteKing)) = True
    | (isSquareEmpty board (char2, int2)) && (int2 > int1) && ((isColor board ((nextChar char1), (int2 - 1)) White) || (isColor board ((nextChar char1), (int2 - 1)) WhiteKing)) = True
    | otherwise = False

{- validJumpWhite board (char1, int1) (char2, int2)
   Checks if a move where you jump over a black piece with a white piece from the coordinates (char1, int1) to the coordinates (char2, int2) in board is valid.
   PRE:   (char1, int1) and (char2, int2) are coordinates that exists in board.
          There is a black piece at coorinates (char1, int1) in board.
          The second coordinate (char2, int2) (to where you want to move a piece) is Empty
   RETURNS: True if the square to jump over is a black piece
            False if the square to jump over is not a black piece
   SIDE EFFECTS: None.
   EXAMPLES:  validMove updatedBoard ('G', 2) ('E', 4) = True
              validMove startBoard ('G', 2) ('E', 4) = False
-}
validJumpWhite :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validJumpWhite board (char1, int1) (char2, int2)
    | (validValue board (char1, int1) (char2, int2) == False) = False
    | (getPiece board (char1, int1) == BlackKing) && ((isSquareEmpty board (char2, int2)) && (int2 < int1) && (isColor board ((prevChar char1), (int2 + 1))) White) || (isColor board ((prevChar char1), (int2 + 1))) WhiteKing = True
    | (getPiece board (char1, int1) == BlackKing) && ((isSquareEmpty board (char2, int2)) && (int2 > int1) && (isColor board ((prevChar char1), (int2 - 1))) White) || (isColor board ((prevChar char1), (int2 - 1))) WhiteKing = True
    | ((isSquareEmpty board (char2, int2)) && (int2 < int1) && (isColor board ((prevChar char1), (int2 + 1))) Black) || (isColor board ((prevChar char1), (int2 + 1))) BlackKing = True
    | ((isSquareEmpty board (char2, int2)) && (int2 > int1) && (isColor board ((prevChar char1), (int2 - 1))) Black) || (isColor board ((prevChar char1), (int2 - 1))) BlackKing = True
    | otherwise = False

{- kingCheck board color
   checks to se is a piece is in the opponents kingsrow
   PRE: True
   RETURNS: True if the piece is in the opponents kingsrow otherwise false
   SIDE EFFECTS: None.
-}
kingCheck :: Checkerboard -> Color -> [(Char, Int)]
kingCheck board color
    | color == Black = blackKingCheck board (board !! 7) 0
    | color == White = whiteKingCheck board (board !! 0) 0
    | otherwise = []

{- blackKingCheck board (x:xs) int
   checks to se is a piece of color black is in the opponents kingsrow
   PRE: True
   RETURNS: the position of all the pieces that are in the opponents kingsrow in a list
   SIDE EFFECTS: None.
-}
blackKingCheck :: Checkerboard -> [Color] -> Int -> [(Char, Int)]
blackKingCheck board [] int = []
blackKingCheck board (x:xs) int = (blackKingCheckAux board [('J', (int + 1))]) ++ blackKingCheck board xs (int +1)
  where
    blackKingCheckAux :: Checkerboard -> [(Char, Int)] -> [(Char, Int)]
    blackKingCheckAux board ((char, int):xs)
        | (getPiece board (char, int)) == Black = [(char, int)]
        | otherwise = []

{- whiteKingCheck board (x:xs) int
   checks to se is a piece of color white is in the opponents kingsrow
   PRE: True
   RETURNS: the position of all the pieces that are in the opponents kingsrow in a list
   SIDE EFFECTS: None.
-}
whiteKingCheck :: Checkerboard -> [Color] -> Int -> [(Char, Int)]
whiteKingCheck board [] int = []
whiteKingCheck board (x:xs) int = (whiteKingCheckAux board [('A', (int +1))]) ++ whiteKingCheck board xs (int + 1)
  where
    whiteKingCheckAux :: Checkerboard -> [(Char, Int)] -> [(Char, Int)]
    whiteKingCheckAux board ((char, int):xs)
        | (getPiece board (char, int)) == White = [(char, int)]
        | otherwise = []


{- placeKing board xs color
   Replaces the coordinate in xs to either a black or a white king in the board.
   PRE: xs contains coordinates within the board.
        color is of data type Color. 
   RETURNS: A checkerboard with every coordinate in xs substituted to a king on the board. 
   SIDE EFFECTS: None.
   EXAMPLES: placeKing startBoard [('G',2)] White == [[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
                                                      [Black,Empty,Black,Empty,Black,Empty,Black,Empty],
                                                      [Empty,Black,Empty,Black,Empty,Black,Empty,Black],
                                                      [Black,Empty,Black,Empty,Black,Empty,Black,Empty],
                                                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                                                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                                                      [Empty,WhiteKing,Empty,White,Empty,White,Empty,White],
                                                      [White,Empty,White,Empty,White,Empty,White,Empty],
                                                      [Empty,White,Empty,White,Empty,White,Empty,White],
                                                      [White,Empty,White,Empty,White,Empty,White,Empty]]
   VARIANT: length xs
-}
placeKing :: Checkerboard -> [(Char, Int)] -> Color -> Checkerboard
placeKing board [] color = board
placeKing board (x:xs) color
    | (color == Black) = placeBlackKing board x
    | (color == White) = placeWhiteKing board x

{- placeBlackKing board (char, int)
   Places a black king piece at coordinate (char, int).
   PRE: (char, int) is a coordinate on the board.
   RETURNS: A checkerboard with the coordinate (char, int) made to a white king piece.
   SIDE EFFECTS: None. 
   EXAMPLES: placeBlackKing startBoard ('E', 5) == [[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
                                                    [Black,Empty,Black,Empty,Black,Empty,Black,Empty],
                                                    [Empty,Black,Empty,Black,Empty,Black,Empty,Black],
                                                    [Black,Empty,Black,Empty,Black,Empty,Black,Empty],
                                                    [Empty,Empty,Empty,Empty,BlackKing,Empty,Empty,Empty],
                                                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                                                    [Empty,White,Empty,White,Empty,White,Empty,White],
                                                    [White,Empty,White,Empty,White,Empty,White,Empty],
                                                    [Empty,White,Empty,White,Empty,White,Empty,White],
                                                    [White,Empty,White,Empty,White,Empty,White,Empty]]
-}
placeBlackKing :: Checkerboard -> (Char, Int) -> Checkerboard
placeBlackKing board (char, int) = take (ord char - 65) board ++ (placePieceAux (board !! (ord char - 65)) int BlackKing):(drop (ord char - 64) board)

{- placeWhiteKing board (char, int)
   Places a white king piece at coordinate (char, int).
   PRE: (char, int) is a coordinate on the board.
   RETURNS: A checkerboard with the coordinate (char, int) made to a white king piece.
   SIDE EFFECTS: None. 
   EXAMPLES: placeWhiteKing startBoard ('E', 5) == [[Empty,Black,Empty,Black,Empty,Black,Empty,Black],
                                                    [Black,Empty,Black,Empty,Black,Empty,Black,Empty],
                                                    [Empty,Black,Empty,Black,Empty,Black,Empty,Black],
                                                    [Black,Empty,Black,Empty,Black,Empty,Black,Empty],
                                                    [Empty,Empty,Empty,Empty,WhiteKing,Empty,Empty,Empty],
                                                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                                                    [Empty,White,Empty,White,Empty,White,Empty,White],
                                                    [White,Empty,White,Empty,White,Empty,White,Empty],
                                                    [Empty,White,Empty,White,Empty,White,Empty,White],
                                                    [White,Empty,White,Empty,White,Empty,White,Empty]]
-}
placeWhiteKing :: Checkerboard -> (Char, Int) -> Checkerboard
placeWhiteKing board (char, int) = take (ord char - 65) board ++ (placePieceAux (board !! (ord char - 65)) int WhiteKing):(drop (ord char - 64) board)

{- validJumpBlackKing board (char1, int1) (char2, int2)
   Validates if a move with black king piece from coordinates (char1, int1) to (char2, int2) is valid on the board.
   PRE: (char1, int1) is an existing coordinate on the board.
   RETURNS: True if a move with black king piece from coordinates (char1, int1) to (char2, int2) is valid.
            False if a move with black king piece from coordinates (char1, int1) to (char2, int2) is not valid.
   SIDE EFFECTS: None. 
   EXAMPLES:  validJumpBlackKing testBoard ('E',2) ('G',4) == True
              validJumpBlackKing testBoard ('E',2) ('G',3) == True
-}
validJumpBlackKing :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validJumpBlackKing board (char1, int1) (char2, int2)
    | (validValue board (char1, int1) (char2, int2) == False) = False

    | (isSquareEmpty board (char2, int2)) && (int2 < int1) && (((isColor board ((prevChar char1), (int2 + 1))) White) ||
      ((isColor board ((prevChar char1), (int2 + 1))) WhiteKing)) ||
      ((isColor board ((nextChar char1), (int2 + 1)) White) ||
      ((isColor board ((nextChar char1), (int2 + 1))) WhiteKing)) = True

    | (isSquareEmpty board (char2, int2)) && (int2 > int1) && (((isColor board ((prevChar char1), (int2 - 1))) White) ||
      ((isColor board ((prevChar char1), (int2 - 1))) WhiteKing)) ||
      ((isColor board ((nextChar char1), (int2 - 1)) White)) ||
      ((isColor board ((nextChar char1), (int2 - 1))) WhiteKing) = True
    | otherwise = False

{- validJumpWhiteKing board (char1, int1) (char2, int2)
   Validates if a move with white king piece from coordinates (char1, int1) to (char2, int2) is valid on the board.
   PRE: (char1, int1) is an existing coordinate on the board.
   RETURNS: True if a move with white king piece from coordinates (char1, int1) to (char2, int2) is valid.
            False if a move with white king piece from coordinates (char1, int1) to (char2, int2) is not valid.
   SIDE EFFECTS: None. 
   EXAMPLES:  validJumpWhiteKing testBoard ('E',2) ('G',4) == True
              validJumpWhiteKing testBoard ('E',2) ('G',3) == True
-}
validJumpWhiteKing :: Checkerboard -> (Char, Int) -> (Char, Int) -> Bool
validJumpWhiteKing board (char1, int1) (char2, int2)
    | (validValue board (char1, int1) (char2, int2) == False) = False

    | (isSquareEmpty board (char2, int2)) && (int2 < int1) && (((isColor board ((prevChar char1), (int2 + 1))) Black) ||
      ((isColor board ((prevChar char1), (int2 + 1))) BlackKing)) ||
      ((isColor board ((nextChar char1), (int2 + 1)) Black) ||
      ((isColor board ((nextChar char1), (int2 + 1))) BlackKing)) = True

    | (isSquareEmpty board (char2, int2)) && (int2 > int1) && (((isColor board ((prevChar char1), (int2 - 1))) Black) ||
      ((isColor board ((prevChar char1), (int2 - 1))) BlackKing)) ||
      ((isColor board ((nextChar char1), (int2 - 1)) Black)) ||
      ((isColor board ((nextChar char1), (int2 - 1))) BlackKing) = True

    | otherwise = False

{- allJumpMovesBlackKing board
   Gets the coordinates of all jump moves for black king pieces in the board. Puts it in a list.
   PRE: True. 
   RETURNS: A list of coordinates, the possible jump moves for black king pieces in the board. 
   SIDE EFFECTS: None.
   EXAMPLES:  allJumpMovesBlackKing startBoard == []
              allJumpMovesBlackKing board == [('F',1), ('F',5)]
-} 
allJumpMovesBlackKing :: Checkerboard -> [(Char, Int)]
allJumpMovesBlackKing board = (allJumpsLeftRightBlack board (getAllPieces board BlackKing)) ++ (allJumpsLeftRightWhite board (getAllPieces board BlackKing))

{- allJumpMovesWhiteKing board
   Gets the coordinates of all jump moves for white king pieces in the board. Puts it in a list.
   PRE: True. 
   RETURNS: A list of coordinates, the possible jump moves for white king pieces in the board. 
   SIDE EFFECTS: None.
   EXAMPLES:  allJumpMovesWhiteKing startBoard == []
              allJumpMovesWhiteKing board == [('F',1), ('F',5)]
-} 
allJumpMovesWhiteKing :: Checkerboard -> [(Char, Int)]
allJumpMovesWhiteKing board = (allJumpsLeftRightBlack board (getAllPieces board WhiteKing)) ++ (allJumpsLeftRightWhite board (getAllPieces board WhiteKing))

{- jumpMoveAvailableBlack board
   Checks of there are any possible moves for the black pieces in the board. 
   PRE: True. 
   RETURNS: True if there are any possible moves for the black pieces in the board. 
            False if there are not any possible moves for the black pieces in the board. 
   SIDE EFFECTS: None.
   EXAMPLES:  jumpMoveAvailableBlack startBoard == []
              jumpMoveAvailableBlack board == True
-} 
jumpMoveAvailableBlack :: Checkerboard -> Bool
jumpMoveAvailableBlack board
    | ((allJumpMovesBlack board == []) && (allJumpMovesBlackKing board == [])) = False
    | otherwise = True

{- allJumpMovesBlack board
   Gets the coordinates of all jump moves for black pieces in the board. Puts it in a list.
   PRE: True. 
   RETURNS: A list of coordinates, the possible jump moves for black pieces in the board. 
   SIDE EFFECTS: None.
   EXAMPLES:  allJumpMovesBlack startBoard == []
              allJumpMovesBlack board == [('F',1), ('F',5)]
-} 
allJumpMovesBlack :: Checkerboard -> [(Char, Int)]
allJumpMovesBlack board = allJumpsLeftRightBlack board (getAllPieces board Black)

{- allJumpsLeftRightBlack board xs
   Gets the coordinates of all jump move to the left and right from coordinates in xs. Puts it in a list.
   PRE: xs consists of existing coordinates on the board. 
   RETURNS: A list of coordinates, the possible jump move to the left and right to make from each element in xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allJumpsLeftRightBlack startBoard (getAllPieces startBoard Black) == [] 
              allJumpsLeftRightBlack board (getAllPieces board Black) == [('F',1), ('F',5)]
   VARIANT: length xs
-} 
allJumpsLeftRightBlack :: Checkerboard -> [(Char, Int)] -> [(Char, Int)]
allJumpsLeftRightBlack board [] = []
allJumpsLeftRightBlack board (x:xs) = jumpsLeftRightBlack board x ++ allJumpsLeftRightBlack board xs

{- jumpsLeftRightBlack board (char, int)
   Gets the coordinate of a jump move to the left and right from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of coordinates, the possible jump move to the left and right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  jumpsLeftRightBlack startBoard ('D',3) == []
              jumpsLeftRightBlack updatedBoard ('D',3) == [('F',1), ('F',5)]
-} 
jumpsLeftRightBlack :: Checkerboard -> (Char, Int) -> [(Char, Int)]
jumpsLeftRightBlack board (char, int) = jumpLeftBlack board (char, int) ++ jumpRightBlack board (char, int)

{- jumpLeftBlack board (char, int)
   Gets the coordinate of a jump move to the left from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible jump move to the left to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  jumpLeftBlack startBoard ('D',3) == []
              jumpLeftBlack updatedBoard ('D',3) == ('F',1)
-} 
jumpLeftBlack :: Checkerboard -> (Char, Int) -> [(Char, Int)]
jumpLeftBlack board (char, int)
    | ((validJumpBlack board (char, int) ((nextChar (nextChar char)), (int - 2))) == True) = [((nextChar (nextChar char)), (int - 2))]
    | otherwise = []

{- jumpRightBlack board (char, int)
   Gets the coordinate of a jump move to the right from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible jump move to the right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  jumpRightBlack startBoard ('D',3) == []
              jumpRightBlack updatedBoard ('D',3) == ('F',5)
-} 
jumpRightBlack :: Checkerboard -> (Char, Int) -> [(Char, Int)]
jumpRightBlack board (char, int)
    | ((validJumpBlack board (char, int) ((nextChar (nextChar char)), (int + 2))) == True) =  [((nextChar (nextChar char)), (int + 2))]
    | otherwise = []

{- jumpMoveAvailableWhite board
   Checks of there are any possible moves for the white pieces in the board. 
   PRE: True. 
   RETURNS: True if there are any possible moves for the white pieces in the board. 
            False if there are not any possible moves for the white pieces in the board. 
   SIDE EFFECTS: None.
   EXAMPLES:  jumpMoveAvailableWhite startBoard == []
              jumpMoveAvailableWhite board == True
-} 
jumpMoveAvailableWhite :: Checkerboard -> Bool
jumpMoveAvailableWhite board
    | ((allJumpMovesWhite board == []) && (allJumpMovesWhiteKing board == [])) = False
    | otherwise = True

{- allJumpMovesWhite board
   Gets the coordinates of all jump moves for white pieces in the board. Puts it in a list.
   PRE: True. 
   RETURNS: A list of coordinates, the possible jump moves for white pieces in the board. 
   SIDE EFFECTS: None.
   EXAMPLES:  allJumpMovesWhite startBoard == []
              allJumpMovesWhite board == [('F',1), ('F',5)]
-} 
allJumpMovesWhite :: Checkerboard -> [(Char, Int)]
allJumpMovesWhite board = allJumpsLeftRightWhite board (getAllPieces board White)

{- allJumpsLeftRightWhite board xs
   Gets the coordinates of all jump move to the left and right from coordinates in xs. Puts it in a list.
   PRE: xs consists of existing coordinates on the board. 
   RETURNS: A list of coordinates, the possible jump move to the left and right to make from each element in xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allJumpsLeftRightWhite startBoard (getAllPieces startBoard White) == [] 
              allJumpsLeftRightWhite board (getAllPieces board White) == [('F',1), ('F',5)]
   VARIANT: length xs
-} 
allJumpsLeftRightWhite :: Checkerboard -> [(Char, Int)] -> [(Char, Int)]
allJumpsLeftRightWhite board [] = []
allJumpsLeftRightWhite board (x:xs) = jumpsLeftRightWhite board x ++ allJumpsLeftRightWhite board xs

{- jumpsLeftRightWhite board (char, int)
   Gets the coordinate of a jump move to the left and right from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of coordinates, the possible jump move to the left and right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  jumpsLeftRightWhite startBoard ('G',2) == []
              jumpsLeftRightWhite updatedBoard ('G',4) == [('E',2), ('E',6)]
-}
jumpsLeftRightWhite :: Checkerboard -> (Char, Int) -> [(Char, Int)]
jumpsLeftRightWhite board (char, int) = jumpLeftWhite board (char, int) ++ jumpRightWhite board (char, int)

{- jumpLeftWhite board (char, int)
   Gets the coordinate of a jump move to the left from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible jump move to the left to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  jumpLeftWhite startBoard ('G',2) == []
              jumpLeftWhite updatedBoard ('G',4) == ('E',2)
-} 
jumpLeftWhite :: Checkerboard -> (Char, Int) -> [(Char, Int)]
jumpLeftWhite board (char, int)
    | ((validJumpWhite board (char, int) ((prevChar (prevChar char)), (int - 2))) == True) = [((prevChar (prevChar char)), (int - 2))]
    | otherwise = []

{- jumpRightWhite board (char, int)
   Gets the coordinate of a jump move to the right from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible jump move to the right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  jumpRightWhite startBoard ('G',2) == []
              jumpRightWhite updatedBoard ('G',2) == ('E',4)
-} 
jumpRightWhite :: Checkerboard -> (Char, Int) -> [(Char, Int)]
jumpRightWhite board (char, int)
    | ((validJumpWhite board (char, int) ((prevChar (prevChar char)), (int + 2))) == True) = [((prevChar (prevChar char)), (int + 2))]
    | otherwise = []

{- allDiagonalMoves board color
   Gets the coordinates of all diagonal moves for either black or white pieces on the board. Puts them in a list.
   PRE: True.
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make for the pieces in the board.
   SIDE EFFECTS: None.
   EXAMPLES:  allDiagonalMoves startBoard Black == [('E',2),('E',2),('E',4),('E',4),('E',6),('E',6),('E',8)]
              allDiagonalMoves [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]] Black = []
              allDiagonalMoves startBoard White == [('F',1),('F',3),('F',3),('F',5),('F',5),('F',7),('F',7)]
              allDiagonalMoves startBoard Empty == []
-} 
allDiagonalMoves :: Checkerboard -> Color -> [(Char, Int)]
allDiagonalMoves board color
    | color == Black = allDiagonalMovesBlack board
    | color == White = allDiagonalMovesWhite board
    | otherwise = []

{- allDiagonalMovesBlack board
   Gets the coordinates of all diagonal moves for black pieces on the board. Puts them in a list.
   PRE: True.
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make for the black pieces in the board.
   SIDE EFFECTS: None.
   EXAMPLES:  allDiagonalMovesBlack startBoard == [('E',2),('E',2),('E',4),('E',4),('E',6),('E',6),('E',8)]
              allDiagonalMovesBlack [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]] = []
              allDiagonalMovesBlack [[]] == []
-} 
allDiagonalMovesBlack :: Checkerboard -> [(Char, Int)]
allDiagonalMovesBlack board = allDiagonalLeftRightBlack board (getAllPieces board Black)

{- allDiagonalLeftRightBlack board xs
   Gets the coordinates of a diagonal move for a black piece to the left and right from every coordinate (char, int) in the list xs. Puts moves in a list.
   PRE: xs contains coordinates existing in the board.
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make from every (char, int) in xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allDiagonalLeftRightBlack startBoard (getAllPieces startBoard Black) == [('E',2),('E',2),('E',4),('E',4),('E',6),('E',6),('E',8)]
              allDiagonalLeftRightBlack startBoard [('G',2),('G',4)] == []
              allDiagonalLeftRightBlack [[]] [] == []
   VARIANT: length xs
-}
allDiagonalLeftRightBlack :: Checkerboard -> [(Char, Int)] -> [(Char, Int)]
allDiagonalLeftRightBlack board [] = []
allDiagonalLeftRightBlack board (x:xs) = diagonalLeftRightBlack board x ++ allDiagonalLeftRightBlack board xs

{- diagonalLeftRightBlack board (char, int)
   Gets the coordinates of a diagonal move for a black piece to the left and right from coordinate (char, int). Puts them in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  diagonalLeftRightBlack startBoard ('D', 6) == [('C',5),('C',7)]
              diagonalLeftRightBlack startBoard ('D', 5) == []
-}  
diagonalLeftRightBlack :: Checkerboard -> (Char, Int) -> [(Char, Int)]
diagonalLeftRightBlack board (char, int) = diagonalLeftBlack board (char, int) ++ diagonalRightBlack board (char, int)

{- diagonalLeftBlack board (char, int)
   Gets the coordinate of a diagonal move to the left from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible diagonal move to the left to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  diagonalLeftBlack startBoard ('D', 6) == [('C',5)]
              diagonalLeftBlack startBoard ('D', 8) == [('C',7)]
-} 
diagonalLeftBlack :: Checkerboard -> (Char, Int) -> [(Char, Int)]
diagonalLeftBlack board (char, int)
    | (validMoveBlack board (char, int) ((nextChar char), (int - 1)) == True) = [((nextChar char), (int - 1))]
    | otherwise = []

{- diagonalRightBlack board (char, int)
   Gets the coordinate of a diagonal move to the right from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible diagonal move to the right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  diagonalRightBlack startBoard ('D', 6) == [('C',7)]
              diagonalRightBlack startBoard ('D', 8) == []
-} 
diagonalRightBlack :: Checkerboard -> (Char, Int) -> [(Char, Int)]
diagonalRightBlack board (char, int)
    | (validMoveBlack board (char, int) ((nextChar char), (int + 1)) == True) = [((nextChar char), (int + 1))]
    | otherwise = []

{- allDiagonalMovesWhite board
   Gets the coordinates of all diagonal moves for white pieces on the board. Puts them in a list.
   PRE: True.
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make for the white pieces in the board.
   SIDE EFFECTS: None.
   EXAMPLES:  allDiagonalMovesWhite startBoard == [('F',1),('F',3),('F',3),('F',5),('F',5),('F',7),('F',7)]
              allDiagonalMovesWhite [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]] = []
              allDiagonalMovesWhite [[]] == []
-} 
allDiagonalMovesWhite :: Checkerboard -> [(Char, Int)]
allDiagonalMovesWhite board = allDiagonalLeftRightWhite board (getAllPieces board White)

{- allDiagonalLeftRightWhite board xs
   Gets the coordinates of a diagonal move for a white piece to the left and right from every coordinate (char, int) in the list xs. Puts moves in a list.
   PRE: xs contains coordinates existing in the board.
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make from every (char, int) in xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allDiagonalLeftRightWhite startBoard (getAllPieces startBoard White) == [('F',1),('F',3),('F',3),('F',5),('F',5),('F',7),('F',7)]
              allDiagonalLeftRightWhite startBoard [('G',2),('G',4)] == [('F',1),('F',3),('F',3),('F',5)]
              allDiagonalLeftRightWhite [[]] [] == []
   VARIANT: length xs
-} 
allDiagonalLeftRightWhite :: Checkerboard -> [(Char, Int)] -> [(Char, Int)]
allDiagonalLeftRightWhite board [] = []
allDiagonalLeftRightWhite board (x:xs) = diagonalLeftRightWhite board x ++ allDiagonalLeftRightWhite board xs

{- diagonalLeftRightWhite board (char, int)
   Gets the coordinates of a diagonal move for a white piece to the left and right from coordinate (char, int). Puts them in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of coordinates, the possible diagonal move to the left and right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  diagonalLeftRightWhite startBoard ('G', 2) == [('F',1),('F',3)]
              diagonalLeftRightWhite startBoard ('G', 3) == []
-}  
diagonalLeftRightWhite :: Checkerboard -> (Char, Int) -> [(Char, Int)]
diagonalLeftRightWhite board (char, int) = diagonalLeftWhite board (char, int) ++ diagonalRightWhite board (char, int)

{- diagonalLeftWhite board (char, int)
   Gets the coordinate of a diagonal move to the left from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible diagonal move to the left to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  diagonalLeftWhite startBoard ('G', 6) == [('F',5)]
              diagonalLeftWhite startBoard ('G', 7) == [('F',6)]
-}  
diagonalLeftWhite :: Checkerboard -> (Char, Int) -> [(Char, Int)]
diagonalLeftWhite board (char, int)
    | (validMoveWhite board (char, int) ((prevChar char), (int - 1)) == True) = [((prevChar char), (int - 1))]
    | otherwise = []


{- diagonalRightWhite board (char, int)
   Gets the coordinate of a diagonal move to the right from coordinate (char, int). Puts it in a list.
   PRE: (char, int) is an existing coordinate on the board. 
   RETURNS: A list of one coordinate, the possible diagonal move to the right to make from (char, int).
   SIDE EFFECTS: None.
   EXAMPLES:  diagonalRightWhite startBoard ('G', 6) == [('F',7)]
              diagonalRightWhite startBoard ('G', 8) == []
-}  
diagonalRightWhite :: Checkerboard -> (Char, Int) -> [(Char, Int)]
diagonalRightWhite board (char, int)
    | (validMoveWhite board (char, int) ((prevChar char), (int + 1)) == True) = [((prevChar char), (int + 1))]
    | otherwise = []

{- getAllPieces board color
   Gets a list of coordinates of given color.
   PRE:   Board contains of lists.
          color is of data type Color.
   RETURNS: A list of coordinates.
   SIDE EFFECTS: None.
   EXAMPLES:  getAllPieces startBoard Black == [('A',2),('A',4),('A',6),('A',8),('B',1),('B',3),('B',5),('B',7),('C',2),('C',4),('C',6),('C',8),('D',1),('D',3),('D',5),('D',7)]
              getAllPieces startBoard BlackKing == []

-}
getAllPieces :: Checkerboard -> Color -> [(Char, Int)]
getAllPieces board color
    | (color == BlackKing) = allPiecesBlackKing board 0 "ABCDEFGHIJ"
    | (color == WhiteKing) = allPiecesWhiteKing board 0 "ABCDEFGHIJ"
    | (color == Black)  = allPiecesBlack board 0 "ABCDEFGHIJ"
    | (color == White)  = allPiecesWhite board 0 "ABCDEFGHIJ"
    | otherwise = []

{- allPiecesBlackKing xs int string
   Collects the coordinates of all black king pieces in the board xs in a list.
   PRE: Board xs contains of lists.
        intForChar and intCoordinate are within the indexes of the board.
   RETURNS: Returns a list of the coordinates with black king pieces in the board xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesBlackKing startBoard 0 "ABCDEFGHIJ" == []
              allPiecesBlackKing board 0 "ABCDEFGHIJ" == [('A', 1)]
              allPiecesBlackKing startBoard 0 "ABCDEFGHIJ" == []
   VARIANT: length xs
-}
allPiecesBlackKing :: Checkerboard -> Int -> String -> [(Char, Int)]
allPiecesBlackKing [] int string = []
allPiecesBlackKing (x:xs) int string = allPiecesBlackKingAux x int string 1 ++ allPiecesBlackKing xs (int + 1) string

{- allPiecesBlackKingAux xs intForChar string intCoordinate
   Checks every element in a list for the ones with color Black King.
   PRE: List xs contains of emelemts of data type Color.
        intForChar and intCoordinate are within the indexes of the board.
   RETURNS: A list of the coordinates with black king pieces in the row xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesBlackKingAux [White, Empty, White, Empty, White, Empty, White, Empty] 0 "AB" 0 == []
              allPiecesBlackKingAux [Empty, BlackKing, Empty, BlackKing, Empty, BlackKing, Empty, BlackKing] 0 "AB" 0 == [('A',1),('A',3),('A',5),('A',7)]
-}
allPiecesBlackKingAux :: [Color] -> Int -> String -> Int -> [(Char, Int)]
allPiecesBlackKingAux [] intForChar string intCoordinate = []
allPiecesBlackKingAux (color:xs) intForChar string intCoordinate
    | (color == BlackKing) = (getCoorinates (string !! intForChar) intCoordinate) ++ allPiecesBlackKingAux xs intForChar string (intCoordinate + 1)
    | otherwise = allPiecesBlackKingAux xs intForChar string (intCoordinate + 1)

{- allPiecesWhiteKing xs int string
   Collects the coordinates of all white king pieces in the board xs in a list.
   PRE: Board xs contains of lists.
        intForChar and intCoordinate are within the indexes of the board.
   RETURNS: Returns a list of the coordinates with white king pieces in the board xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesWhiteKing startBoard 0 "ABCDEFGHIJ" == []
              allPiecesWhiteKing board 0 "ABCDEFGHIJ" == [('A', 1)]
              allPiecesWhiteKing startBoard 0 "ABCDEFGHIJ" == []
   VARIANT: length xs
-}
allPiecesWhiteKing :: Checkerboard -> Int -> String -> [(Char, Int)]
allPiecesWhiteKing [] int string = []
allPiecesWhiteKing (x:xs) int string = allPiecesWhiteKingAux x int string 1 ++ allPiecesWhiteKing xs (int + 1) string

{- allPiecesWhiteKingAux xs intForChar string intCoordinate
   Checks every element in a list for the ones with color White King.
   PRE: List xs contains of emelemts of data type Color.
        intForChar and intCoordinate are within the indexes of the board.
   RETURNS: A list of the coordinates with white king pieces in the row xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesWhiteKingAux [WhiteKing, Empty, WhiteKing, Empty, WhiteKing, Empty, WhiteKing, Empty] 0 "AB" 0 == [('A',0),('A',2),('A',4),('A',6)]
              allPiecesWhiteKingAux [WhiteKing, Empty] 2 "ABC" 1 == [('C',1)]

-}
allPiecesWhiteKingAux :: [Color] -> Int -> String -> Int -> [(Char, Int)]
allPiecesWhiteKingAux [] intForChar string intCoordinate = []
allPiecesWhiteKingAux (color:xs) intForChar string intCoordinate
    | (color == WhiteKing) = (getCoorinates (string !! intForChar) intCoordinate) ++ allPiecesWhiteKingAux xs intForChar string (intCoordinate + 1)
    | otherwise = allPiecesWhiteKingAux xs intForChar string (intCoordinate + 1)

{- allPiecesBlack xs int string
   Collects the coordinates of all black pieces in the board xs in a list.
   PRE: Board xs contains of lists.
        intForChar and intCoordinate are within the indexes of the board.
   RETURNS: Returns a list of the coordinates with black pieces in the board xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesBlack startBoard 0 "ABCDEFGHIJ" == [('A',2),('A',4),('A',6),('A',8),('B',1),('B',3),('B',5),('B',7),('C',2),('C',4),('C',6),('C',8),('D',1),('D',3),('D',5),('D',7)]
              allPiecesBlack board 0 "ABCDEFGHIJ" == [('A', 1)]
              allPiecesBlack startBoard 0 "ABCDEFGHIJ" == []
   VARIANT: length xs
-}
allPiecesBlack :: Checkerboard -> Int -> String -> [(Char, Int)]
allPiecesBlack [] int string = []
allPiecesBlack (x:xs) int string = allPiecesBlackAux x int string 1 ++ allPiecesBlack xs (int + 1) string

{- allPiecesBlackAux xs intForChar string intCoordinate
   Checks every element in a list for the ones with color Black.
   PRE: List xs contains of emelemts of data type Color.
        intForChar and intCoordinate are within the indexes of the board.
   RETURNS: A list of the coordinates with black pieces in the row xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesBlackAux [White, Empty, White, Empty, White, Empty, White, Empty] 0 "AB" 0 == []
              allPiecesBlackAux [Empty, Black, Empty, Black, Empty, Black, Empty, Black] 0 "AB" 0 == [('A',1),('A',3),('A',5),('A',7)]
-}
allPiecesBlackAux :: [Color] -> Int -> String -> Int -> [(Char, Int)]
allPiecesBlackAux [] intForChar string intCoordinate = []
allPiecesBlackAux (color:xs) intForChar string intCoordinate
    | (color == Black) = (getCoorinates (string !! intForChar) intCoordinate) ++ allPiecesBlackAux xs intForChar string (intCoordinate + 1)
    | otherwise = allPiecesBlackAux xs intForChar string (intCoordinate + 1)

{- allPiecesWhite xs int string
   Collects the coordinates of all white pieces in the board xs in a list.
   PRE: Board xs contains of lists.
   RETURNS: Returns a list of the coordinates with white pieces in the board xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesWhite startBoard 0 "ABCDEFGHIJ" == [('G',2),('G',4),('G',6),('G',8),('H',1),('H',3),('H',5),('H',7),('I',2),('I',4),('I',6),('I',8),('J',1),('J',3),('J',5),('J',7)]
              allPiecesWhite board 0 "ABCDEFGHIJ" == [('A', 1)]
              allPiecesWhite startBoard 0 "ABCDEFGHIJ" == []
   VARIANT: length xs
-}
allPiecesWhite :: Checkerboard -> Int -> String -> [(Char, Int)]
allPiecesWhite [] int string = []
allPiecesWhite (x:xs) int string = allPiecesWhiteAux x int string 1 ++ allPiecesWhite xs (int + 1) string

{- allPiecesWhiteAux xs intForChar string intCoordinate
   Checks every element in a list for the ones with color White.
   PRE: List xs contains of emelemts of data type Color.
   RETURNS: A list of the coordinates with white pieces in the row xs.
   SIDE EFFECTS: None.
   EXAMPLES:  allPiecesWhiteAux [White, Empty, White, Empty, White, Empty, White, Empty] 0 "AB" 0 == [('A',0),('A',2),('A',4),('A',6)]
              allPiecesWhiteAux [White, Empty] 2 "ABC" 1 == [('C',1)]

-}
allPiecesWhiteAux :: [Color] -> Int -> String -> Int -> [(Char, Int)]
allPiecesWhiteAux [] intForChar string intCoordinate = []
allPiecesWhiteAux (color:xs) intForChar string intCoordinate
    | (color == White) = (getCoorinates (string !! intForChar) intCoordinate) ++ allPiecesWhiteAux xs intForChar string (intCoordinate + 1)
    | otherwise = allPiecesWhiteAux xs intForChar string (intCoordinate + 1)


{- getCoordinates char int
   Puts the char and the int in the format of a coordinate, puts it in a list. 
   PRE: The char and int are values within the board.
   RETURNS: A list of one coordinate.
   SIDE EFFECTS: None.
   EXAMPLES:  getCoordinates 'A' 1 == [('A', 1)]
              getCoordinates 'B' 2 == [('B', 2)]
-}
getCoorinates :: Char -> Int -> [(Char, Int)]
getCoorinates char int = [(char, int)]


{- win board color
   Checks in one color has won.
   PRE:  color is either Black or White.
   RETURNS: True if anyone has won. 
            False if no one has won.
   SIDE EFFECTS: None.
   EXAMPLES:  win startBoard Black == False
              win updatedBoard Black == True 
-}
win :: Checkerboard -> Color -> Bool
win board color
    | color == Black && ((getAllPieces board White) == [] && (getAllPieces board WhiteKing == [])) = True
    | color == White && ((getAllPieces board Black) == [] && (getAllPieces board BlackKing == [])) = True
    | otherwise = False


{- isSquareEmpty board (char, int)
   Checks if the square at coordinates (char, int) in board is Empty
   PRE:  (char, int) are coordinates that exists in the board
   RETURNS: True if the square at coordinates (char, int) in the board is Empty
            False if the square at coordinates (char, nt) in the board is Empty
   SIDE EFFECTS: None.
   EXAMPLES:  isSquareEmpty startBoard ('A', 1) == True
              isSquareEmpty startBoard ('A', 2) == False
-}
isSquareEmpty :: Checkerboard -> (Char, Int) -> Bool
isSquareEmpty board (char, int)
    | (isColor board (char, int) Empty) == True = True
    | otherwise = False

{- nextChar char
   Gets the next character in the ASCII table.
   PRE:  char is a character in the ASCII table.
   RETURNS: A char of a character in the ASCII table.
   SIDE EFFECTS: None
   EXAMPLES:  nextChar 'A' == 'B'
              nextChar 'B' == 'C'
-}
nextChar :: Char -> Char
nextChar char = chr (ord char + 1)

{- prevChar char
   Gets the previous character in the ASCII table.
   PRE:  char is a character in the ASCII table.
   RETURNS: A char of a character in the ASCII table.
   SIDE EFFECTS: None
   EXAMPLES:  prevChar 'A' == '@'
              prevChar 'B' == 'A'
              prevChar 'C' == 'B'
-}
prevChar :: Char -> Char
prevChar char = chr (ord char - 1)

--------------------------------------------------------------------------------
-- TEST

testBoard = [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
              [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
              [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
              [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
              [Empty, Black, Empty, Empty, Empty, Empty, Empty, Empty],
              [Empty, Empty, White, Empty, Empty, Empty, Empty, Empty],
              [Empty, White, Empty, Empty, Empty, White, Empty, White],
              [White, Empty, White, Empty, White, Empty, White, Empty],
              [Empty, White, Empty, White, Empty, White, Empty, White],
              [White, Empty, White, Empty, White, Empty, White, Empty] ]

kingTestBoard = [ [Empty, Empty, Empty, Empty, Empty, WhiteKing, Empty, Empty],
                  [Empty, Empty, Black, Empty, White, Empty, Empty, Empty],
                  [Empty, Black, Empty, Black, Empty, Empty, Empty, Empty],
                  [Empty, Empty, Black, Empty, Empty, Empty, Empty, Empty],
                  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                  [Empty, Empty, White, Empty, Empty, Empty, Empty, Empty],
                  [Empty, White, Empty, Empty, Empty, Empty, Empty, White],
                  [White, Empty, Empty, Empty, Empty, Empty, White, Empty],
                  [Empty, Empty, Empty, BlackKing, Empty, Empty, Empty, Empty],
                  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ]


-- boardToString
test111 = TestCase $ assertEqual "boardToString [[Black]] 0 AB" "A |b|\n" (boardToString [[Black]] 0 "AB")
test112 = TestCase $ assertEqual "boardToString [[White]] 0 AB" "A |w|\n" (boardToString [[White]] 0 "AB")
test113 = TestCase $ assertEqual "boardToString [[Empty, Black, Empty, Black, Empty, Black, Empty, Black]] 1 AB" "B |_|b|_|b|_|b|_|b|\n" (boardToString [[Empty, Black, Empty, Black, Empty, Black, Empty, Black]] 1 "AB")

-- boardToStringAux
test121 = TestCase $ assertEqual "boardToStringAux [Black]" "|b|\n" (boardToStringAux [Black])
test122 = TestCase $ assertEqual "boardToStringAux [White]" "|w|\n" (boardToStringAux [White])
test123 = TestCase $ assertEqual "boardToStringAux [Empty, Black, Empty, Black, Empty, Black, Empty, Black]" "|_|b|_|b|_|b|_|b|\n" (boardToStringAux [Empty, Black, Empty, Black, Empty, Black, Empty, Black])

-- updateBoard
test131 = TestCase $ assertEqual "updateBoard black pice move" [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                [Empty, Black, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                [White, Empty, White, Empty, White, Empty, White, Empty],
                                                                [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                [White, Empty, White, Empty, White, Empty, White, Empty] ]
                                                                (updateBoard [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                               [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                               [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                               [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                               [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                               [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                               [White, Empty, White, Empty, White, Empty, White, Empty],
                                                                               [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                               [White, Empty, White, Empty, White, Empty, White, Empty] ] ('D',1) ('E',2))
test132 = TestCase $ assertEqual "updateBoard white pice jump" [  [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                  [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                  [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                  [White, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                  [Empty, Black, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                  [Empty, White, Empty, Empty, Empty, White, Empty, White],
                                                                  [White, Empty, White, Empty, White, Empty, White, Empty],
                                                                  [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                  [White, Empty, White, Empty, White, Empty, White, Empty] ]
                                                               (updateBoard testBoard ('F',3) ('D',1))

-- removePiece
test141 = TestCase $ assertEqual "removePiece [[Empty,Black,Empty]] ('A', 2)" [[Empty,Empty,Empty]] (removePiece [[Empty,Black,Empty]] ('A', 2))

-- placePiece
test151 = TestCase $ assertEqual "placePiece [[Black, Empty, Black]] ('A', 1) ('A',2)" [[Black,Black,Black]] (placePiece [[Black, Empty, Black]] ('A', 1) ('A',2))

-- getPiece
test161 = TestCase $ assertEqual "getPiece startBoard ('A', 1)" Empty (getPiece startBoard ('A', 1))
test162 = TestCase $ assertEqual "getPiece startBoard ('B', 1)" Black (getPiece startBoard ('B', 1))

-- isColor
test171 = TestCase $ assertEqual "isColor startBoard ('A', 1) Black" False (isColor startBoard ('A', 1) Black)
test172 = TestCase $ assertEqual "isColor startBoard ('A', 1) Empty" True (isColor startBoard ('A', 1) Empty)

-- validValue
test201 = TestCase $ assertEqual "validValue startBoard ('A', 1) ('A',1)" True (validValue startBoard ('A', 1) ('A',1))
test202 = TestCase $ assertEqual "validValue startBoard ('A', 1) ('A',9)" False (validValue startBoard ('A', 1) ('A',9))
test203 = TestCase $ assertEqual "validValue startBoard ('A', 1) ('K',1)" False (validValue startBoard ('A', 1) ('K',1))

-- validMove
test211 = TestCase $ assertEqual "validMove startBoard ('D', 1) ('E', 2) Black" True (validMove startBoard ('D', 1) ('E', 2) Black)
test212 = TestCase $ assertEqual "validMove startBoard ('D', 1) ('E', 1) Black" False (validMove startBoard ('D', 1) ('E', 1) Black)
test213 = TestCase $ assertEqual "validMove startBoard ('D', 2) ('E', 2) White" False (validMove startBoard ('D', 2) ('E', 2) White)

-- validMoveBlack
test221 = TestCase $ assertEqual "validMoveBlack startBoard ('D', 1) ('E', 2)" True (validMoveBlack startBoard ('D', 1) ('E', 2))
test222 = TestCase $ assertEqual "validMoveBlack startBoard ('D', 1) ('E', 1)" False (validMoveBlack startBoard ('D', 1) ('E', 1))

-- validMoveWhite
test231 = TestCase $ assertEqual "validMoveWhite startBoard ('G', 2) ('F', 1)" True (validMoveWhite startBoard ('G', 2) ('F', 1))
test232 = TestCase $ assertEqual "validMoveWhite startBoard ('G', 2) ('F', 2)" False (validMoveWhite startBoard ('G', 2) ('F', 2))

-- correctDiagonalWhite
test241 = TestCase $ assertEqual "correctDiagonalWhite startBoard ('G', 2) ('F', 1)" True (correctDiagonalWhite startBoard ('G', 2) ('F', 1))
test242 = TestCase $ assertEqual "correctDiagonalWhite startBoard ('G', 2) ('F', 2)" False (correctDiagonalWhite startBoard ('G', 2) ('F', 2))

-- correctDiagonalBlack
test251 = TestCase $ assertEqual "correctDiagonalBlack startBoard ('D', 1) ('E', 2)" True (correctDiagonalBlack startBoard ('D', 1) ('E', 2))
test252 = TestCase $ assertEqual "correctDiagonalBlack startBoard ('D', 1) ('E', 1)" False (correctDiagonalBlack startBoard ('D', 1) ('E', 1))

-- validJumpBlack
test261 = TestCase $ assertEqual "validJumpBlack testBoard ('E', 2) ('G', 4)" True (validJumpBlack testBoard ('E', 2) ('G', 4))
test262 = TestCase $ assertEqual "validJumpBlack startBoard ('D', 1) ('F', 3)" False (validJumpBlack startBoard ('D', 1) ('F', 3))

-- validJumpWhite
test271 = TestCase $ assertEqual "validJumpWhite testBoard ('F', 3) ('D', 1)" True (validJumpWhite testBoard ('F', 3) ('D', 1))
test272 = TestCase $ assertEqual "validJumpWhite startBoard ('G', 4) ('E', 2)" False (validJumpWhite startBoard ('G', 4) ('E', 2))

-- removeJumpVictim
test281 = TestCase $ assertEqual "removeJumpedPiece testBoard ('F', 3) ('D', 1)" [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                                   [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                                   [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                                   [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                                   [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                                   [Empty, Empty, White, Empty, Empty, Empty, Empty, Empty],
                                                                                   [Empty, White, Empty, Empty, Empty, White, Empty, White],
                                                                                   [White, Empty, White, Empty, White, Empty, White, Empty],
                                                                                   [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                                   [White, Empty, White, Empty, White, Empty, White, Empty] ] (removeJumpedPiece testBoard ('F', 3) ('D', 1))
test282 = TestCase $ assertEqual "removeJumpedPiece testBoard ('E', 2) ('G', 4)" [ [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                                   [Black, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                                   [Empty, Black, Empty, Black, Empty, Black, Empty, Black],
                                                                                   [Empty, Empty, Black, Empty, Black, Empty, Black, Empty],
                                                                                   [Empty, Black, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                                   [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                                                                                   [Empty, White, Empty, Empty, Empty, White, Empty, White],
                                                                                   [White, Empty, White, Empty, White, Empty, White, Empty],
                                                                                   [Empty, White, Empty, White, Empty, White, Empty, White],
                                                                                   [White, Empty, White, Empty, White, Empty, White, Empty] ] (removeJumpedPiece testBoard ('E', 2) ('G', 4))

--test31 = TestCase $ assertEqual "" _ ()

tests = TestList [TestLabel "Test boardToString1" test111,        TestLabel "Test boardToString2" test112,    TestLabel "Test boardToString3" test113,
                  TestLabel "Test boardToStringAux1" test121,     TestLabel "Test boardToStringAux2" test122, TestLabel "Test boardToStringAux3" test123,
                  TestLabel "Test updateBoard1" test131,          TestLabel "Test updateBoard2" test132,
                  TestLabel "Test removePiece1" test141,
                  TestLabel "Test placePiece1" test151,
                  TestLabel "Test getPiece1" test161,             TestLabel "Test getPiece2" test162,
                  TestLabel "Test isColor1" test171,              TestLabel "Test isColor2" test172,
                  TestLabel "Test validValue1" test201,           TestLabel "Test validValue2" test202,       TestLabel "Test validValue3" test203,
                  TestLabel "Test validMove1" test211,            TestLabel "Test validMove2" test212,        TestLabel "Test validMove3" test213,
                  TestLabel "Test validMoveBlack1" test221,       TestLabel "Test validMoveBlack2" test222,
                  TestLabel "Test validMoveWhite2" test231,       TestLabel "Test validMoveWhite2" test232,
                  TestLabel "Test correctDiagonalWhite1" test241, TestLabel "Test correctDiagonalWhite2" test242,
                  TestLabel "Test correctDiagonalBlack1" test251, TestLabel "Test correctDiagonalBlack2" test252,
                  TestLabel "Test validJumpBlack1" test261,       TestLabel "Test validJumpBlack2" test262,
                  TestLabel "Test validJumpWhite1" test271,       TestLabel "Test validJumpWhite2" test272,
                  TestLabel "Test removeJumpVictim1" test281,     TestLabel "Test removeJumpVictim2" test282]
