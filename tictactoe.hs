-- TicTacToe

-- We'll be storing the tic-tac-toe board in a list of lists (a matrix), and
-- parsing strings into tuples in order to figure out where the user wants to
-- place their next piece. So we need to import the Data list and character packages.
import Data.List
import Data.Char

-- We need to be able to handle user input exceptions!
import Control.Exception

-- Just need this package to be able to clear the screen between player moves
import System.Console.ANSI

-- Need this package to deal with arguments to main
import System.Environment

-- Need this to allow user to quit the program if they decide not to play again
import System.Exit

-- Setting up our tic-tac-toe board data structures
type Matrix = [[Int]]
type Position = (Int,Int)

-- Our Xs and Os are represented by 1s and 2s. User is 1, Computer is 2
-- The starting board will only have zeroes.
userNum = 1
compNum = 2
startingBoard = [[0,0,0],[0,0,0],[0,0,0]]

-- To run the program, we can just run "main E" or "main H".
-- Any other argument will run in Easy mode by default
main :: IO ()
main = do args <- getArgs
          case args of
             ["H"] -> do clearScreen
                         putStrLn "Playing in hard mode"
                         playGame startingBoard "H"
             ["E"] -> do clearScreen
                         putStrLn "Playing in easy mode"
                         playGame startingBoard "E"
             _ -> do clearScreen
                     putStrLn "Arguments not understood. Playing in default easy mode."
                     playGame startingBoard "E"

-- Main program flow:
--
-- Show current board state
-- Check for win or tie
----If there was a win or tie, figure out if the user wants to play again.
-- Ask user to make a move
-- Check for win or tie
-- Computer makes a move
-- This is a recursive function

playGame :: Matrix -> String -> IO ()
playGame board difficultyLevel = do putStrLn "Here's what the board looks like:\n"
                                    showBoardState board
                                    -- check for Wins and Ties after each move
                                    checkWinsTies board
                                    (x, y) <- getUserMove
                                    case checkUserMove board (x, y) of
                                                  [[3]] -> do clearScreen
                                                              putStrLn "There's already a piece there! Try another spot on the board."
                                                              playGame board difficultyLevel
                                                  [[4]] -> do clearScreen
                                                              putStrLn "Hmm, I can't find that spot on the board. Maybe you made a typo? Try again."
                                                              playGame board difficultyLevel
                                                  [[5]] -> do clearScreen
                                                              putStrLn "Something strange happened. Let's try again."
                                                              playGame board difficultyLevel
                                                -- If user inputs something other than H or E when running main, we'll by default
                                                -- play in easy mode.
                                                -- Check for wins and ties after each move
                                                  a -> case difficultyLevel of
                                                            "H" -> do clearScreen
                                                                      checkWinsTies a
                                                                      (playGame (getHardComputerMove (a)) difficultyLevel)
                                                            "E" -> do clearScreen
                                                                      checkWinsTies a
                                                                      (playGame (getEasyComputerMove (a)) difficultyLevel)
                                                            otherwise -> do clearScreen
                                                                            checkWinsTies a
                                                                            (playGame (getEasyComputerMove (a)) "E")

-- Once the game is over, check if the user wants to play again
playAgain :: IO()
playAgain = do putStrLn "Would you like to play again? Y/N"
               answer <- getLine
               case answer of
                  "Y" -> do whichMode
                  "N" -> do putStrLn "Thanks for playing!"
                            exitSuccess
                  otherwise -> do putStrLn "I didn't really understand that..come again?"
                                  playAgain

-- Ask if user would like to play in Easy or Hard mode
whichMode :: IO()
whichMode = do putStrLn "Would you like to play in easy or hard mode? E/H"
               answer <- getLine
               case answer of
                   "E" -> do clearScreen
                             playGame startingBoard "E"
                   "H" -> do clearScreen
                             playGame startingBoard "H"
                   otherwise -> do putStrLn "I didn't really understand that..come again?"
                                   whichMode

-- Get move coordinates from User (Row, Column)
-- Need exception handling here to validate Tuple input
getUserMove :: IO (Int, Int)
getUserMove = do putStrLn "Your move! A few pointers: "
                 putStrLn "- Use the format (row number, column number): "
                 putStrLn "- Rows start from zero from top to bottom."
                 putStrLn "- Columns start from zero from left to right."
                 putStrLn "- Es are empty spaces on the board.\n\n"
                 tuple <- getLine
                 readTuple <- try ((readIO tuple)) :: IO (Either SomeException (Int, Int))
                 case readTuple of
                   Left _ -> do putStrLn "I didn't understand that...try again?"
                                getUserMove
                   Right _ -> do return (read tuple)


-- Check for wins and ties
checkWinsTies :: Matrix -> IO ()
checkWinsTies board = if (wasThereAWin board compNum)
                         then do putStrLn "The computer won this time...better luck next time!"
                                 playAgain
                      else if (wasThereAWin board userNum)
                         then do putStrLn "You won...congratulations!"
                                 playAgain
                      else if (wasThereATie board)
                         then do putStrLn "It's a tie!"
                                 playAgain
                      else do return ()

-- Functions for manipulating the tictactoe board:
----- checkPiece
----- performMove
----- boardSurgery
----- changeHead
----- doMove
----- checkUserMove
----- getHardComputerMove
----- getEasyComputerMove

-- What is the value at a certain location on the board?
checkPiece :: Matrix -> Position -> Int
checkPiece board (y, x) = board !! y !! x

-- Perform a move for the user or computer on specified location on the board.
-- We'll increment the zero a certain amount (userNum or compNum)
performMove :: Position -> (a->a) -> [[a]] -> [[a]]
performMove (y, x) f = boardSurgery y (\ycoord -> boardSurgery x f ycoord)

-- Helper function for getMove, splits up the board Row in question,
-- modifies requested spot, and then puts the Row back together
boardSurgery :: Int -> (a->a) -> [a] -> [a]
boardSurgery x a as = let (before, after) = splitAt x as
  in before ++ changeHead a after

-- Helper function for boardSurgery - modifies the head of a list
-- This helps us place a piece at the spot chosen by the user or computer
changeHead :: (a->a) -> [a] -> [a]
changeHead _ [] = []
changeHead f (a:as) = f a : as

-- Validate move. Perform it, if valid, otherwise return error code
doMove :: (Int, Int) -> Int -> Matrix -> Matrix
doMove (a, b) computerOrUser board
  | ((checkPiece board (a, b)) == 0) = ((performMove (a, b) (\a->a+computerOrUser) board))
  | (checkPiece board (a, b) == compNum) = [[0]] -- Signal there's already a piece on the board
  | (checkPiece board (a, b) == userNum) = [[0]] -- Signal there's already a piece on the board
  | otherwise = [[1]] -- Signal there was an input error of some kind

-- Helper function for doMove for the user
-- Figure out what to do with user move coordinates
checkUserMove :: Matrix -> Position -> Matrix
checkUserMove board tuple = do
                       if (((doMove (tuple) userNum board) /= [[0]]) && ((doMove (tuple) userNum board) /= [[1]]))
                                  then do (doMove (tuple) userNum board)
                       else if ((doMove (tuple) userNum board) == [[0]])
                          then do [[3]]
                       else if ((doMove (tuple) userNum board) == [[1]])
                          then do [[4]]
                       else do [[5]]

-- Helper method for doMove, calculating computer's next move (hard)
getHardComputerMove :: Matrix -> Matrix
getHardComputerMove board = doMove (computerHardNextMove board) compNum board

-- Helper method for doMove, calculating computer's next move (easy)
getEasyComputerMove :: Matrix -> Matrix
getEasyComputerMove board = doMove (computerEasyNextMove board) compNum board



-- Functions for displaying the tictactoe board:
----- printRow
----- printMatrix
----- showBoardState

-- Helper function for printMatrix - pretty prints a board row
printRow :: [Int] -> String
printRow [] = "\n"
printRow [a]
    | (a == compNum) = show "O"
    | (a == userNum) = show "X"
    | otherwise = show "E"
printRow (a:as) = (printRow [a]) ++ " " ++ (printRow as) ++ "\n"

-- Pretty prints all the board rows
printMatrix :: Matrix -> String
printMatrix [] = "\n\n"
printMatrix (a:as) = printRow a ++ printMatrix as

-- Pretty prints the tic-tac-toe board
showBoardState :: Matrix -> IO ()
showBoardState board = do putStr (printMatrix board)




-- Functions for evaluating the state of the tictactoe board
----- wasThereAWin
----- wasThereATie


-- Detect Win. Where Int should equal userNum for the user, and compNum for the computer
wasThereAWin :: Matrix -> Int -> Bool
wasThereAWin [a,b,c] d
  | all (\y->(y==d)) a = True
  | all (\y->(y==d)) b = True
  | all (\y->(y==d)) c = True
  | all (\y->(y==d)) (map (!! 0) [a,b,c]) = True
  | all (\y->(y==d)) (map (!! 1) [a,b,c]) = True
  | all (\y->(y==d)) (map (!! 2) [a,b,c]) = True
  | all (\y->(y==d)) [a !! 0, b !! 1, c !! 2] = True
  | all (\y->(y==d)) [a !! 2, b !! 1, c !! 0] = True
  | otherwise = False

-- Detect Tie.
wasThereATie :: Matrix -> Bool
wasThereATie [a,b,c]
  | ([findEmptySpace a, findEmptySpace b, findEmptySpace c] == [4,4,4]) = True
  | otherwise = False


-- Logic for calculating computer's next move (easy and hard)
----- computerHardNextMove
----- computerEasyNextMove


-- Computer's automated move - computer plays to win and also to prevent user from winning
-- Evaluating blocking must come after evaluating possible wins, but before other possible moves
computerHardNextMove :: Matrix -> Position
computerHardNextMove [a,b,c]
  | (rowWinner [a,b,c] /= (4,4)) = rowWinner [a,b,c]
  | (verticalWinner [a,b,c] /= (4,4)) = verticalWinner [a,b,c]
  | (diagonalWinner [a,b,c] /= (4,4)) = diagonalWinner [a,b,c]
  | ((blockUser [a,b,c]) /= (4,4)) = (blockUser [a,b,c])
  | (rowStrategicNext [a,b,c] /= (4,4)) = rowStrategicNext [a,b,c]
  | (diagonalStrategicNext [a,b,c] /= (4,4)) = diagonalStrategicNext [a,b,c]
  | (verticalStrategicNext [a,b,c] /= (4,4)) = verticalStrategicNext [a,b,c]
  | (emptyNext [a,b,c] /= (4,4)) = emptyNext [a,b,c]
  | otherwise = (4,4) -- This should never be returned - There will only be no empty spaces if the board is full, and a tie will be detected prior to calculating the computer's next move

-- Computer plays only to maximize its own chances of winning
-- but doesn't actively block user
computerEasyNextMove :: Matrix -> Position
computerEasyNextMove [a,b,c]
  | (rowWinner [a,b,c] /= (4,4)) = rowWinner [a,b,c]
  | (verticalWinner [a,b,c] /= (4,4)) = verticalWinner [a,b,c]
  | (diagonalWinner [a,b,c] /= (4,4)) = diagonalWinner [a,b,c]
  | (rowStrategicNext [a,b,c] /= (4,4)) = rowStrategicNext [a,b,c]
  | (diagonalStrategicNext [a,b,c] /= (4,4)) = diagonalStrategicNext [a,b,c]
  | (verticalStrategicNext [a,b,c] /= (4,4)) = verticalStrategicNext [a,b,c]
  | (emptyNext [a,b,c] /= (4,4)) = emptyNext [a,b,c]
  | otherwise = (4,4) -- This should never be returned - There will only be no empty spaces if the board is full, and a tie will be detected prior to calculating the computer's next move




-- Does the computer have an opportunity for a horizontal win?
rowWinner :: Matrix -> Position
rowWinner [a,b,c]
  | ((rowNextWinningMove a compNum 0) /= (4,4)) = (rowNextWinningMove a compNum 0)
  | ((rowNextWinningMove b compNum 1) /= (4,4)) = (rowNextWinningMove b compNum 1)
  | ((rowNextWinningMove c compNum 2) /= (4,4)) = (rowNextWinningMove c compNum 2)
  | otherwise = (4,4)

rowNextWinningMove :: [Int] -> Int -> Int -> Position
rowNextWinningMove a d rowIndex
  | (a == [d,d,0]) = (rowIndex, 2)
  | (a == [d,0,d]) = (rowIndex, 1)
  | (a == [0,d,d]) = (rowIndex, 0)
  | otherwise = (4,4) -- Signal that we didn't find a winning move in this row




-- Does the computer have an opportunity for a vertical win?
verticalWinner :: Matrix -> Position
verticalWinner [a,b,c]
 | ((verticalNextWinningMove [a,b,c] 0 compNum) /= (4,4)) = (verticalNextWinningMove [a,b,c] 0 compNum)
 | ((verticalNextWinningMove [a,b,c] 1 compNum) /= (4,4)) = (verticalNextWinningMove [a,b,c] 1 compNum)
 | ((verticalNextWinningMove [a,b,c] 2 compNum) /= (4,4)) = (verticalNextWinningMove [a,b,c] 2 compNum)
 | otherwise = (4,4)

verticalNextWinningMove :: Matrix -> Int -> Int -> Position
verticalNextWinningMove [a,b,c] column d
 | ([a !! column, b !! column, c !! column] == [d,d,0]) = (2,column)
 | ([a !! column, b !! column, c !! column] == [d,0,d]) = (1,column)
 | ([a !! column, b !! column, c !! column] == [0,d,d]) = (0,column)
 | otherwise = (4,4) -- Signal that we didn't find a winning move in this column




 -- Does the computer have an opportunity for a diagonal win?
diagonalWinner :: Matrix -> Position
diagonalWinner [a,b,c]
  | ((diagonalNextWinningMove [a,b,c] compNum) /= (4,4)) = (diagonalNextWinningMove [a,b,c] compNum)
  | otherwise = (4,4)

diagonalNextWinningMove :: Matrix -> Int -> Position
diagonalNextWinningMove [a,b,c] d
  | ((all (\y->(y==d)) [a !! 0, b !! 1]) && (c !! 2 == 0)) = (2, 2)
  | ((all (\y->(y==d)) [a !! 0, c !! 2]) && (b !! 1 == 0)) = (1, 1)
  | ((all (\y->(y==d)) [b !! 1, c !! 2]) && (a !! 0 == 0)) = (0, 0)
  | ((all (\y->(y==d)) [a !! 2, b !! 1]) && (c !! 0 == 0)) = (2, 0)
  | ((all (\y->(y==d)) [c !! 0, b !! 1]) && (a !! 2 == 0)) = (0, 2)
  | ((all (\y->(y==d)) [a !! 2, c !! 0]) && (b !! 1 == 0)) = (1, 1)
  | otherwise = (4,4) -- Signal that we didn't find a winning move in either of the diagonal cases




-- Find a strategic empty slot next to one of the computer's pieces in a row
rowStrategicNext :: Matrix -> Position
rowStrategicNext [a,b,c]
  | (findGoodEmptySpaceRow a compNum /= 4) = (0, (findGoodEmptySpaceRow a compNum))
  | (findGoodEmptySpaceRow b compNum /= 4) = (1, (findGoodEmptySpaceRow b compNum))
  | (findGoodEmptySpaceRow c compNum /= 4) = (2, (findGoodEmptySpaceRow c compNum))
  | otherwise = (4,4)

findGoodEmptySpaceRow :: [Int] -> Int -> Int
findGoodEmptySpaceRow x y = case elemIndex 0 x of
                       Just n -> case elemIndex y x of
                                    Just o -> case (abs(n-o)) of
                                                  1 -> n
                                                  otherwise -> 4 -- Signal that we didn't find a strategic empty slot
                                    Nothing -> 4 -- Signal that we didn't find a strategic empty slot
                       Nothing -> 4 -- Signal that we didn't find a strategic empty slot




-- Find a strategic empty slot diagonal to one of the computer's pieces
diagonalStrategicNext :: Matrix -> Position
diagonalStrategicNext [a,b,c]
  | (findGoodEmptySpaceDiagonal [a,b,c] compNum /= (4,4)) = (findGoodEmptySpaceDiagonal [a,b,c] compNum)
  | otherwise = (4,4)

findGoodEmptySpaceDiagonal :: Matrix -> Int -> Position
findGoodEmptySpaceDiagonal [a,b,c] d
  | ((all (\y->(y==d)) [a !! 0]) && (b !! 1 == 0) && (c !! 2 == 0)) = (1, 1)
  | ((all (\y->(y==d)) [c !! 2]) && (b !! 1 == 0) && (a !! 0 == 0)) = (1, 1)
  | ((all (\y->(y==d)) [c !! 0]) && (b !! 1 == 0) && (a !! 2 == 0)) = (1, 1)
  | ((all (\y->(y==d)) [a !! 2]) && (b !! 1 == 0) && (c !! 0 == 0)) = (1, 1)
  | ((all (\y->(y==d)) [b !! 1]) && (a !! 2 == 0) && (c !! 0 == 0)) = (0, 2)
  | ((all (\y->(y==d)) [b !! 1]) && (a !! 0 == 0) && (c !! 2 == 0)) = (2, 2)
  | otherwise = (4,4) -- Signal that we didn't find a strategic move in any of the diagonal cases




-- Find a strategic empty vertical slot next to one of the computer's pieces
verticalStrategicNext :: Matrix -> Position
verticalStrategicNext [a,b,c]
  | (verticalNextGoodEmptySpace [a,b,c] 0 compNum /= (4,4)) = (verticalNextGoodEmptySpace [a,b,c] 0 compNum)
  | (verticalNextGoodEmptySpace [a,b,c] 1 compNum /= (4,4)) = (verticalNextGoodEmptySpace [a,b,c] 1 compNum)
  | (verticalNextGoodEmptySpace [a,b,c] 2 compNum /= (4,4)) = (verticalNextGoodEmptySpace [a,b,c] 2 compNum)
  | otherwise = (4,4)

verticalNextGoodEmptySpace :: Matrix -> Int -> Int -> Position
verticalNextGoodEmptySpace [a,b,c] column d
  | ([a !! column, b !! column, c !! column] == [d,0,0]) = (2,column)
  | ([a !! column, b !! column, c !! column] == [0,0,d]) = (1,column)
  | ([a !! column, b !! column, c !! column] == [0,d,0]) = (0,column)
  | otherwise = (4,4) -- Signal that we didn't find a winning move in this column




-- If no strategic moves can be made, just have the computer look for an empty space
emptyNext :: Matrix -> Position
emptyNext [a,b,c]
  | (findEmptySpace a /= 4) = (0, findEmptySpace a)
  | (findEmptySpace b /= 4) = (1, findEmptySpace b)
  | (findEmptySpace c /= 4) = (2, findEmptySpace c)
  | otherwise = (4,4)

findEmptySpace :: [Int] -> Int
findEmptySpace x = case elemIndex 0 x of
                     Just n -> n
                     Nothing -> 4 -- Signal that we didn't find any empty spaces on the board - must be a tie




-- Is the user about to win? If so, return the space they need so the computer will block them.
blockUser :: Matrix -> Position
blockUser [a,b,c]
  | ((rowNextWinningMove a userNum 0) /= (4,4)) = (rowNextWinningMove a userNum 0)
  | ((rowNextWinningMove b userNum 1) /= (4,4)) = (rowNextWinningMove b userNum 1)
  | ((rowNextWinningMove c userNum 2) /= (4,4)) = (rowNextWinningMove c userNum 2)
  | ((verticalNextWinningMove [a,b,c] 0 userNum) /= (4,4)) = (verticalNextWinningMove [a,b,c] 0 userNum)
  | ((verticalNextWinningMove [a,b,c] 1 userNum) /= (4,4)) = (verticalNextWinningMove [a,b,c] 1 userNum)
  | ((verticalNextWinningMove [a,b,c] 2 userNum) /= (4,4)) = (verticalNextWinningMove [a,b,c] 2 userNum)
  | ((diagonalNextWinningMove [a,b,c] userNum) /= (4,4)) = (diagonalNextWinningMove [a,b,c] userNum)
  | otherwise = (4,4)
