module Battleship where

import qualified Data.Text.IO  as T
import           Text.Tabl

type Coordinate = (Char, Int)
type Ship = [Coordinate]
type Field = [[Bool]]

validX = ['A' .. 'J']
validY = [1 .. 10]

water_val = 0
ship_val = 1
miss_val = 2
hit_val = 3

{-
-- Probably also want a player type

-- Initialize the play field
initField :: Field

-- Extract the coordinate from the string
convertInputToCoords :: String -> Coordinate

convertInputToCoords h:t = (h,t) -- need to convert t to Int somehow

-- Split a string containing coordinates separated by semi-colons into a list of coordinates
splitCoordsInString :: String -> [String]

-- Check if a coordinate lies inside the field
validateCoords :: Coordinate -> Bool

validateCoords (x,y) = (elem x validX) && (elem y validY)

-- Make sure that the ship is given valid coordinates (in a row/col, no overlap with existing ships)
-- ie for a ship 3 long, make sure it receives 3 coords that are in the same "row"
-- might want to have data about "rows" when we initialize the field
-- input: already placed ships, current ship (coords), length of current ship
validateShipCoordinates :: [Ship] -> Ship -> Int -> Bool

validateShipCoordinates h:t ship len = (validShip h ship len) && (validateShipCoordinates t ship len)

-- validShip is a helper function for validateShipCoordinates that makes sure the 2 given ships do not 
-- inhabit the same coordinates and ensures the second ship is given valid coordinates
-- input: already placed ship, current ship, length of current ship
validShip :: Ship -> Ship -> Int -> Bool

validShip = 

-- Output the field in the terminal (as some printable String)
printField :: Field -> [Ship] -> IO ()

-- Mark a cell on the field as hit
markHit :: Field -> Coordinate -> Field

-- Mark a cell on the field as miss
markMiss :: Field -> Coordinate -> Field

-- Remove the ships from list of ships on field when they are destroyed
removeDestroyedShips :: [Ship] -> [Ship]

-- Check if the ship has been destroyed and remove it from the game if it is
checkShipDestroyed :: Field -> Ship -> Coordinate -> Bool

-- Fire a shot at a given coordinate
-- input: opponent field, list of ships, position to shoot at
-- output: updated field, list of ships, hit or miss
fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)

-- Input a ship with a given length
-- already placed ships, length of ship to place, IO output
inputShip :: [Ship] -> Int -> IO Ship

-- Input all the ships for a player
inputShips :: Int -> [Ship] -> IO [Ship]

-}
{------------------------------- Validation Functions -------------------------------------}



{------------------------------- Print Functions ------------------------------------------}
-- TODO Test the Print Functions
-- printBoards prints the board state
printBoards :: [[Int]] -> [[Int]] -> Bool -> IO ()
printBoards aiBoard playerBoard aiBoardVisible =
  do
    printBoard aiBoard aiBoardVisible
    putStrLn("--------------")
    printBoard playerBoard True


-- printBoard prints the state of a singular board
printBoard :: [[Int]] -> Bool -> IO ()
printBoard board isShipVisibile =
  do
    T.putStrLn $ tabl EnvAscii DecorAll DecorAll [] formatBoard
    where
        formattedBoard = formatBoard board isShipVisible

headerRow = [" ":"A":"B":"C":"D":"E":"F":"G":"H":"I":"J"]
-- formats [[Int]] into [[Text]] to be outputted to console
formatBoard :: [[Int]] -> Bool -> [[Text]]
formatBoard board isShipVisible = headerRow :
                                        [(intToText i):(intListToTextList (board !! (i)) isShipVisible) | i <- [0..9]]

-- convert Int to Text
intToText :: Int -> Text
intToText i = toEnum i :: Text

-- List of integers to a row of corresponding text characters
intListToTextList :: [Int] -> Bool -> [Text]
rowToSymbol row isShipVisible = map (\ i -> getSymbol x isShipVisible) row

-- Integer to a Symbol representing water, ship, miss, hit
-- only show ship if isShipVisible is true, otherwise show water
getboardsymbol :: Int -> Bool -> Text
getboardsymbol i isShipVisible
    | i == 0    = "~" -- water
    | i == 1    = if isShipVisible then "#" else "~" -- ship
    | i == 2    = "O" -- miss
    | i == 3    = "X" -- hit

{------------------------------- Helper Functions -------------------------------------------}

-- setUpPlayerBoard
-- toInt converts a string to an integer assuming its a digit
toInt :: String -> Int
toInt str = toIntHelper 0 str

toIntHelper :: Int -> String -> Int
toIntHelper n [] = n
toIntHelper n (h:t)
    | isDigit h = toIntHelper (n*10 + ch2dig h) t

-- ch2dig converts Char to Int
ch2dig :: Char -> Int
ch2dig ch = fromIntegral (fromEnum ch - fromEnum '0')

-- isDigit check to see if a Char is a digit
isDigit :: Char -> Bool
isDigit ch = ch >=  '0' &&  ch <=  '9'

-- setUpAIBoard

-- ai next moves function, takes difficulty, board, current next moves, last move made, if it was a hit
-- assuming that the choosen target from the previous move has been removed from currNextMoves
getAiNextMoves :: Int -> [[Int]] -> (Int, Int) -> Bool -> [(Int, Int)]
getAiNextMoves 1 board currNextMoves lastMove _ = getRandomMove
getAiNextMoves 4 board currNextMoves lastMove _ = currNextMoves
getAiNextMoves _ board currNextMoves lastMove False
    | null currNextMoves = getRandomMove
    | otherwise = currNextMoves
getAiNextMoves _ board currNextMoves lastMove True = getNextMovesHelper board lastMove

-- generate neighbouring coords around the hit, return the ones that aren't hit/miss
getNextMovesHelper :: [[Int]] -> (Int, Int) -> [(Int, Int)]
getNextMovesHelper board hit = validateNextMoves board (getNeighbours hit)

-- get the surrounding four neighbours
getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

-- make sure neighbours are in the board and are either ships or boards
validateNextMoves :: [(Int, Int)] -> [[Int]] -> [(Int, Int)]
validateNextMoves board neighbours = filter p2 (filter p1 neighbours)
  where p1 n = isValidCoordinateNum n
        p2 n = isWaterOrShip n board

-- Returns true if the given coordinate in the form (row,col) is on the board (ie valid), else false.
isValidCoordinateNum :: (Int, Int) -> Bool
isValidCoordinateNum (row, col) = row >= 0 && row <= 9 && col >= 0 && col <= 9

-- Returns true if the given coordinate is water or ship (has not been targeted yet)
isWaterOrShip :: (Int, Int) -> [[Int]] -> Bool
isWaterOrShip (i,j) board =
    val /= miss_val && val /= hit_val
    where val = getValueAtCoordinate board (i,j)

-- Returns the value on the board at the given coordinate (water,ship,hit,miss)
getValueAtCoordinate :: [[Int]] -> (Int, Int) -> Int
getValueAtCoordinate board (row,col) = (board !! row) !! col


-- getMoves takes an Int that represents the difficulty and a board and returns the initial list of coordinates
-- for the ai
getMoves :: Int -> [[Int]] -> [(Int, Int)]
getMoves 1 board = getRandomMove
getMoves 2 board = getRandomMove
getMoves 3 board = getRandomShipCoord board
getMoves 4 board = getAllShipCoord board
getMoves _ board = [(0,0)]

-- getRandomMove returns a random coordinate between (0,0) and (9,9)
getRandomMove :: [(Int, Int)] 
getRandomMove = 
  do
    g <- newStdGen
    i <- randomRs (0, 9) g
    j <- randomRs (0, 9) g
    return ([(i,j)])


-- getRandomShipCoord returns a random coordinate on the board that stores a ship coordinate
getRandomShipCoord :: [[Int]] -> [(Int, Int)]
getRandomShipCoord board = 
  do
    g <- newStdGen
    return ((getAllShipCoord board)!!(randomRs (0,  (length (getAllShipCoord board)) - 1 ) g))

-- getAllShipCoord returns all the coordinates on the board that stores a ship coordinate
getAllShipCoord :: [[Int]] -> [(Int, Int)]
getAllShipCoord board = getAllShipCoordHelper 0 board

-- outer loop
getAllShipCoordHelper :: Int -> [[Int]] -> [(Int, Int)]
getAllShipCoordHelper _ [] = []
getAllShipCoordHelper row (h:t) = getAllShipCoordHelperHelper row 0 h ++ getAllShipCoordHelper (row+1) t

-- inner loop
getAllShipCoordHelperHelper :: Int -> Int -> [Int] -> [(Int, Int)]
getAllShipCoordHelperHelper _ [] = []
getAllShipCoordHelperHelper row col (h:t)
    | h == 1 || h == 3 = (row,col): getAllShipCoordHelperHelper row (col+1) t
    | otherwise = getAllShipCoordHelperHelper row (col+1) t


-- toCoord takes a list of Ints and returns a list of coords
toCoord :: [Int] -> [(Int,Int)]
toCoord [] = []
toCoord (h1:(h2:t)) = (h1,h2): toCoord t
toCoord (h:[]) = []

-- getAItarget returns the head of aiNextMoves (aiNextMoves shouldn't be empty)
getAItarget :: [(Int,Int)] -> (Int,Int)
getAItarget [] = (0,0)
getAItarget (h:t) = h
-- importBoard

-- allShipsHit returns true if all ships have been hit on the given board, false otherwise
allShipsHit :: [[Int]] -> Bool
allShipsHit [] = True
allShipsHit (h:t) =  (allShipsHitHelper h) && (allShipsHit t)

-- allShipsHitHelper takes a list of Ints and returns true if all the ships in the list have been hit 
--(ie contains no 1's)
allShipsHitHelper :: [Int] -> Bool
allShipsHitHelper [] = True
allShipsHitHelper (h:t)
    | h == 1 = False
    | otherwise = allShipsHitHelper t

-- shipHasBeenHit compares the old board with the new one to see if a ship has been hit
shipHasBeenHit :: [[Int]] -> [[Int]] -> Bool
shipHasBeenHit [] [] = False
shipHasBeenHit (h1:t1) (h2:t2) = (beenHitHelper h1 h2) || (shipHasBeenHit t1 t2)

beenHitHelper :: [Int] -> [Int] -> Bool
beenHitHelper [] [] = False
beenHitHelper (h1:t1) (h2:t2)
    | shipHitHere h1 h2 = True
    | otherwise = beenHitHelper t1 t2

shipHitHere :: Int -> Int -> Bool
shipHitHere 1 3 = True
shipHitHere _ _ = False



-- If input is an invalid coordinate or a coordinate already hit in past turns,
-- recursively call getTarget until a valid target
getTarget :: [[Int]] -> IO (Int, Int)
getTarget aiboard =
    do
        putStrLn("Please input your target in the form (A,1)")
        target <- getLine
        if (isValidCoordinate target)
            then do
                let coordinate = createCoordinate target
                if (isWaterOrShip aiboard coordinate)
                    then return coordinate
                    else do
                        putStrLn("This coordinate was previously hit. Please try again")
                        getTarget aiboard
        else do
            putStrLn("Invalid coordinate form. Please try again")
            getTarget aiboard

-- Checks if the coordinate given is a valid board coordinate.
isValidCoordinate :: [Char] -> Bool
isValidCoordinate [letter, num] = ((toUpper letter) `elem` validX) && (num `elem` validY)
isValidCoordinate lst = False

-- Converting the letter,number representation to a coordinate of form (row, column)
createCoordinate :: [Char] -> (Int, Int)
createCoordinate [letter,num] = ((charToNum num), (convertLetterToNum (toUpper letter)))

-- Takes letter character of a coordinate and returns the integer mapping
convertLetterToNum :: Char -> Int
convertLetterToNum letter
    | letter == 'A' = 0
    | letter == 'B' = 1
    | letter == 'C' = 2
    | letter == 'D' = 3
    | letter == 'E' = 4
    | letter == 'F' = 5
    | letter == 'G' = 6
    | letter == 'H' = 7
    | letter == 'I' = 8
    | letter == 'J' = 9
    | otherwise = -1
{------------------------------- Main Functions -------------------------------------------}

-- play Plays the game
{-
Takes the player board, the ai board, the ai (function that generates next moves),
 the ai board visibility and returns an output (current state of the game)
-}
play :: [[Int]] -> [[Int]] -> Int -> Bool -> [(Int,Int)] -> IO ()

play playerBoard aiBoard difficulty aiBoardVisible aiNextMoves =
  do
    printBoards aiBoard playerBoard aiBoardVisible
    putStrLn("It's your turn, input a coordinate to strike.")
    playerTarget <- getTarget aiBoard
    newAIBoard <- updateBoard aiBoard playerTarget

    if (allShipsHit newAIBoard)
      then do
        putStrLn("Congratulations you have won the match!!!")
    else do
      aiTarget <- getAItarget aiNextMoves
      newPlayerBoard <- updateBoard playerBoard aiTarget

      if (allShipsHit newPlayerBoard)
        then do
          putStrLn("Sorry, looks like you lost...")
      else do
        putStrLn("Would you like to save? (y/n)")
        shouldSave <- getLine
        if (shouldSave == "Y" || shouldSave == "y")
          then do
            save playerBoard aiBoard difficulty aiBoardVisible aiNextMoves
        else do
          play newPlayerBoard newAIBoard ai aiBoardVisible newAiNextMoves difficulty


-- save Saves the game a .csv file
-- first line is [ai difficulty, is aiboard visible]
-- second line is the ai's current list of next moves
-- after that is 10 lines of AI board state and 10 lines of player board state
save :: [[Int]] -> [[Int]] -> Int -> Bool -> [Int] -> IO()
save playerBoard aiBoard difficulty aiBoardVisible aiNextMoves =
  do
    putStrLn("What is the name of the file you'd like to save to? (include .csv)")
    fileName <- getLine
    writeFile fileName (encodeInputs playerBoard aiBoard difficulty aiBoardVisible aiNextMoves)
    putStrLn("The game has benn save in "++fileName++".csv")
    return ()

-- encodeInputs encodes the input into a list of strings
-- encodeInputs playerBoard aiBoard difficulty aiBoardVisible aiNextMoves
encodeInputs :: [[Int]] -> [[Int]] -> Int -> Bool -> [Int] -> String
encodeInputs pBoard aiBoard diff vis aiNext = (encodeAI diff vis) ++ (encodeMoves aiNext) ++ (encodeBoard pBoard) ++ (encodeBoard aiBoard)

-- helper functions to make encode easier
encodeAI :: Int -> Bool -> String
encodeAI difficulty True = show difficulty ++ "," ++ "True" ++ "\n"
encodeAI difficulty False = show difficulty ++ "," ++ "False" ++ "\n"

encodeMoves :: [(Int,Int)] -> String
encodeMoves [] = "\n"
encodeMoves [(h1,h2)] = show h1 ++ "," ++ show h2 ++ "\n"
encodeMoves ((h1,h2):t) = show h1 ++ "," ++ show h2 ++ "," ++  encodeMoves t

encodeBoard :: [[Int]] -> String
encodeBoard [] = ""
encodeBoard (h:t) = (intercalate "," (map show h)) ++ "\n" ++ encodeBoard t

intercalate :: [a] -> [[a]] -> [a]
intercalate s [] = []
intercalate s [x] = x
intercalate s (h:t) = h:s:(intercalate s t)

-- The entry point of the program
main :: IO ()

main =
  do
    putStrLn("Welcome to battleship in haskell, let's get started... ")
    putStrLn("Would you like to start a new game or load a pre-existing file?")
    putStrLn("[1] - New Game, [2] - Load Game")
    newOrLoad <- getLine

    if (newOrLoad == "1") -- new game
      then do
        putStrLn("Ok, lets start a new game.")
        putStrLn("First off lets set up your board")
        putStrLn("Use the format 'A1' 'D6' etc when inputting coordinates for the ships")
          let aiBoardVisible = True
          playerBoard <- setUpPlayerBoard
          putStrLn("Please choose the AI Difficulty:")
          putStrLn("[1] - Easy, [2] - Normal, [3] - Hard, [4] - God")
          difficulty <- getDifficulty
          aiBoard <- setUpAIBoard
          aiNextMoves <- getMoves difficulty
          play playerBoard aiBoard difficulty aiBoardVisible aiNextMoves
    else do -- load game
      putStrLn("What is your file's name?")
      fileName <- getLine
      file <- readFile fileName

      -- first line is [ai difficulty, is aiboard visible]
      -- second line is the ai's current list of next moves
      -- after that is 10 lines of AI board state and 10 lines of player board state
      parsedFile <- [splitsep (==',') line | line <- splitsep (=='\n') file]
      difficulty <- (toInt ((parsedFile !! 0) !! 0))
      aiBoardVisible <- (((parsedFile !! 0) !! 1) == "True")
      aiNextMoves <- (toCoord (map toInt (parsedFile !! 1)))
      aiData <- slice 2 10 parsedFile
      playerData <- slice 12 10 parsedFile
      aiBoard <- importBoard aiData
      playerBoard <- importBoard playerData
      play playerBoard aiBoard difficulty aiBoardVisible aiNextMoves












