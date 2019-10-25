module Battleship where

import qualified Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Random
import Text.Tabl

validX = ['A' .. 'J']
validY = [0 .. 9]

water_val = 0
ship_val = 1
miss_val = 2
hit_val = 3


{------------------------------- Validation Functions -------------------------------------}

-- isInt determines whether a string can be a valid int
isInt :: String -> Bool
isInt [] = True
isInt (h:t) = isDigit h && isInt t







{------------------------------- Print Functions ------------------------------------------}
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
    T.putStrLn $ tabl EnvAscii hdecor vdecor aligns formattedBoard
    where
        hdecor = DecorAll
        vdecor = DecorAll
        aligns = []
        formattedBoard = formatBoard board isShipVisibile

headerRow = ((T.singleton ' '):[T.singleton a | a <- ['A'..'J']])
-- formats [[Int]] into [[T.Text]] to be outputted to console
formatBoard :: [[Int]] -> Bool -> [[T.Text]]
formatBoard board isShipVisible = headerRow :
                                        [(intToText i):(intListToTextList (board !! (i)) isShipVisible) | i <- [0..9]]

-- convert Int to T.Text
intToText :: Int -> T.Text
intToText i = T.pack (show i)

-- List of integers to a row of corresponding text characters
intListToTextList :: [Int] -> Bool -> [T.Text]
intListToTextList row isShipVisible = map (\ x -> getBoardSymbol x isShipVisible) row

-- Integer to a Symbol representing water, ship, miss, hit
-- only show ship if isShipVisible is true, otherwise show water
getBoardSymbol :: Int -> Bool -> T.Text
getBoardSymbol i isShipVisible
    | i == 0    = T.pack "~" -- water
    | i == 1    = if isShipVisible then T.pack "#" else T.pack "~" -- ship
    | i == 2    = T.pack "O" -- miss
    | i == 3    = T.pack "X" -- hit
    | otherwise = T.pack " "











{------------------------------- Helper Functions -------------------------------------------}

-- getDifficulty prompts the player for a difficulty
getDifficulty :: IO Int
getDifficulty =
    do
      putStrLn("Please choose the AI Difficulty:")
      putStrLn("[1] - Easy, [2] - Normal, [3] - Hard, [4] - God")
      difficulty <- getLine
      if (isInt difficulty)
        then do
          return (toInt difficulty)
      else do
        putStrLn("That is not a valid Integer, please try again")
        getDifficulty


-- toInt converts a string to an integer assuming its a digit
toInt :: String -> Int
toInt str = toIntHelper 0 str

toIntHelper :: Int -> String -> Int
toIntHelper n [] = n
toIntHelper n (h:t)
    | isDigit h = toIntHelper (n*10 + charToDig h) t

-- charToDig converts Char to Int
charToDig :: Char -> Int
charToDig ch = fromIntegral (fromEnum ch - fromEnum '0')

-- isDigit check to see if a Char is a digit
isDigit :: Char -> Bool
isDigit ch = ch >=  '0' &&  ch <=  '9'

-- setUpPlayerBoard sets up the playerBoard
setUpPlayerBoard :: IO [[Int]]
setUpPlayerBoard =
    do
      let board = replicate 10 (replicate 10 0)
      board <- placeShip 5 board
      printBoard board True
      board <- placeShip 4 board
      printBoard board True
      board <- placeShip 3 board
      printBoard board True
      board <- placeShip 2 board
      printBoard board True
      placeShip 1 board

-- setUpAIBoard sets up the aiBoard
setUpAIBoard :: IO [[Int]]
setUpAIBoard =
    do
      let board = replicate 10 (replicate 10 0)
      board <- randomlyPlaceShip 5 board
      board <- randomlyPlaceShip 4 board
      board <- randomlyPlaceShip 3 board
      board <- randomlyPlaceShip 2 board
      board <- randomlyPlaceShip 1 board
      return board

-- placeShip n b , manually places a ship of length n on board b (use placeShipHelper)
placeShip :: Int -> [[Int]] -> IO [[Int]]
placeShip 0 b = return b
placeShip n b =
  do
    -- get coord and direction and see if a ship can be place, if so then place otherwise try again
    putStrLn("Please input your ship start point in the form A1")
    start <- getLine
    if (isValidCoordinate start)
        then do
        let startCoord = createCoordinate start
        putStrLn("Please input your ship direction as a number")
        putStrLn("[1] - up, [2] - down, [3] - left [4] - right")
        dir <- getLine
        if (isInt dir && (toInt dir) > 0 &&  (toInt dir) < 5)
            then do
            let endCoord = getEndCoord startCoord n (toInt dir)
            if (isValidCoordinateNum endCoord && isFreeBetween startCoord endCoord b)
                then do
                    return (placeShipHelper startCoord endCoord b)
                else do
                    putStrLn("Invalid ship placement please try again.")
                    placeShip n b
            else do
            putStrLn("Invalid direction please try again.")
            placeShip n b
        else do
        putStrLn("Invalid coordinate please try again.")
        placeShip n b

-- isFreeBetween checks to see if all places in between 2 coords are free for ships to be placed
-- must be in same row or col (guarenteed because we use getEndCoord)
isFreeBetween :: (Int, Int) -> (Int, Int) -> [[Int]] -> Bool
isFreeBetween (s1,s2) (t1,t2) board
    | s1 == t1 && s2 == t2 = isFreeSpace (s1,s2) board
    | s1 == t1 && s2 > t2 = (isFreeSpace (s1,s2) board) && (isFreeBetween (s1,(s2-1)) (t1,t2) board)
    | s1 == t1 && s2 < t2 = (isFreeSpace (s1,s2) board) && (isFreeBetween (s1,(s2+1)) (t1,t2) board)
    | s2 == t2 && s1 > t1 = (isFreeSpace (s1,s2) board) && (isFreeBetween ((s1-1),s2) (t1,t2) board)
    | s2 == t2 && s1 < t1 = (isFreeSpace (s1,s2) board) && (isFreeBetween ((s1+1),s2) (t1,t2) board)
    | otherwise = False

-- isFreeSpace checks to see if a space is free on a board
isFreeSpace :: (Int,Int) -> [[Int]] -> Bool
isFreeSpace (r,c) board = (getValueOfCoordinate board (r,c)) == 0

-- placeShipHelper returns a board with the specified ship added to it
{-
looks something like:
placeShipHelper startCoord endCoord currBoard

- startCoord endCoord are both either on same row or column
- the space between the 2 coordinates are guarenteed to be empty
- placeShipHelper should fill the spaces between startCoord and endCoord (inclusive)
  with ship values
-}
placeShipHelper :: (Int, Int) -> (Int, Int) -> [[Int]] -> [[Int]]
placeShipHelper (s1,s2) (e1,e2) board -- TODO
    | s1 == e1 && s2 == e2 = [[if i == s1 && j == s2 then 1 else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]
    | s1 == e1 && s2 > e2 = [[if i == s1 && j <= s2 && j >= e2 then 1 else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]
    | s1 == e1 && s2 < e2 = [[if i == s1 && j >= s2 && j <= e2 then 1 else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]
    | s2 == e2 && s1 > e1 = [[if j == s2 && i <= s1 && i >= e1 then 1 else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]
    | s2 == e2 && s1 < e1 = [[if j == s2 && i >= s1 && i <= e1 then 1 else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]
    | otherwise = board


-- randomlyPlaceShip
randomlyPlaceShip :: Int -> [[Int]] -> IO [[Int]]
randomlyPlaceShip n b =
  do
    g <- newStdGen
    let startCoord = ( ((randomRs (0,9) g) !! 0), ((randomRs (0,9) g) !! 1) )
    let dir = (randomRs (1,4) g) !! 0
    let endCoord = getEndCoord startCoord n dir
    if (isValidCoordinateNum endCoord && isFreeBetween startCoord endCoord b)
        then do
        return (placeShipHelper startCoord endCoord b)
        else do
        randomlyPlaceShip n b


-- getEndCoord given startCoord n dir returns an end coord
getEndCoord :: (Int,Int) -> Int -> Int -> (Int,Int)
getEndCoord (p1,p2) n 1 = (p1, p2 - (n-1)) -- up
getEndCoord (p1,p2) n 2 = (p1, p2 + (n-1)) -- down
getEndCoord (p1,p2) n 3 = (p1 + (n-1), p2) -- left
getEndCoord (p1,p2) n 4 = (p1 - (n-1), p2) -- right


-- ai next moves function, takes difficulty, board, current next moves, last move made
-- assuming that the choosen target from the previous move has been removed from currNextMoves
getAiNextMoves :: Int -> [[Int]] -> [(Int, Int)] -> (Int, Int) -> Bool -> [(Int, Int)]
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
validateNextMoves :: [[Int]] -> [(Int, Int)] -> [(Int, Int)]
validateNextMoves board neighbours = filter p2 (filter p1 neighbours)
  where p1 n = isValidCoordinateNum n
        p2 n = isWaterOrShip board n

-- Returns true if the given coordinate in the form (row,col) is on the board (ie valid), else false.
isValidCoordinateNum :: (Int, Int) -> Bool
isValidCoordinateNum (row, col) = row >= 0 && row <= 9 && col >= 0 && col <= 9

-- Returns true if the given coordinate is water or ship (has not been targeted yet)
isWaterOrShip :: [[Int]] -> (Int, Int) -> Bool
isWaterOrShip board (i,j) =
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
getRandomMove = let g = mkStdGen 2
 in [head [(i, j) | i <- randomRs (0, 9) g, j <- randomRs (0, 9) g]]


-- getRandomShipCoord returns a random coordinate on the board that stores a ship coordinate
getRandomShipCoord :: [[Int]] -> [(Int, Int)]
getRandomShipCoord board = let g = mkStdGen 2
  in [(getAllShipCoord board)!! (head (randomRs (0,  (length (getAllShipCoord board)) - 1 ) g))]

-- getAllShipCoord returns all the coordinates on the board that stores a ship coordinate
getAllShipCoord :: [[Int]] -> [(Int, Int)]
getAllShipCoord board = getAllShipCoordHelper 0 board

-- outer loop
getAllShipCoordHelper :: Int -> [[Int]] -> [(Int, Int)]
getAllShipCoordHelper _ [] = []
getAllShipCoordHelper row (h:t) = getAllShipCoordHelperHelper row 0 h ++ getAllShipCoordHelper (row+1) t

-- inner loop
getAllShipCoordHelperHelper :: Int -> Int -> [Int] -> [(Int, Int)]
getAllShipCoordHelperHelper _ _ [] = []
getAllShipCoordHelperHelper row col (h:t)
    | h == 1 || h == 3 = (row,col): getAllShipCoordHelperHelper row (col+1) t
    | otherwise = getAllShipCoordHelperHelper row (col+1) t


-- toCoord takes a list of Ints and returns a list of coords
toCoord :: [Int] -> [(Int,Int)]
toCoord [] = []
toCoord (h1:(h2:t)) = (h1,h2):(toCoord t)
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

-- Updates board to reflect player target
-- the target is water or a ship (ie isWaterOrShip returns true)
updateBoard :: [[Int]] -> (Int, Int) -> IO [[Int]]
updateBoard board target =
  do
    if (isWaterOrShip board target)
      then do
        putStrLn("It's a HIT!")
        return (updateBoardSquare board target)
      else do
        putStrLn("It's a miss...")
        return (updateBoardSquare board target)

-- add 2 to the value at position (row,col)
updateBoardSquare :: [[Int]] -> (Int,Int) -> [[Int]]
updateBoardSquare board (row, col) =
    [[if i == row && j == col then (getValueOfCoordinate board (i,j))+2
                              else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]

-- Checks whether the given coordinate contains an unhit ship
unhitShipAtCoord :: [[Int]] -> (Int, Int) -> Bool
unhitShipAtCoord aiboard coordinate = (getValueOfCoordinate aiboard coordinate) == 1

-- Returns the value on the board at the given coordinate
getValueOfCoordinate :: [[Int]] -> (Int, Int) -> Int
getValueOfCoordinate board (row,col) = (board !! row) !! col

-- If input is an invalid coordinate or a coordinate already hit in past turns,
-- recursively call getTarget until a valid target
getTarget :: [[Int]] -> IO (Int, Int)
getTarget aiboard =
    do
        putStrLn("Please input your target in the form A1")
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
isValidCoordinate [letter, num] = ((strToChar (T.unpack (T.toUpper (T.singleton letter)))) `elem` validX) && ((charToDig num) `elem` validY)
isValidCoordinate lst = False

-- Converting the letter,number representation to a coordinate of form (row, column)
createCoordinate :: [Char] -> (Int, Int)
createCoordinate [letter,num] = ((charToDig num), (convertLetterToNum (strToChar (T.unpack (T.toUpper (T.singleton letter))))))

-- takes the first character in string and returns as char
strToChar :: String -> Char
strToChar [] = ' '
strToChar (h:t) = h

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

-- removes head of a list if it exists
removeHead :: [a] -> [a]
removeHead [] = []
removeHead (h:t) = t





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
      let aiTarget = getAItarget aiNextMoves
      newPlayerBoard <- updateBoard playerBoard aiTarget
      let newAiNextMoves = getAiNextMoves difficulty newPlayerBoard (removeHead aiNextMoves) aiTarget (shipHasBeenHit playerBoard newPlayerBoard)

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
          play newPlayerBoard newAIBoard difficulty aiBoardVisible newAiNextMoves


-- save Saves the game a .csv file
-- first line is [ai difficulty, is aiboard visible]
-- second line is the ai's current list of next moves
-- after that is 10 lines of AI board state and 10 lines of player board state
save :: [[Int]] -> [[Int]] -> Int -> Bool -> [(Int,Int)] -> IO()
save playerBoard aiBoard difficulty aiBoardVisible aiNextMoves =
  do
    putStrLn("What is the name of the file you'd like to save to? (Include .csv)")
    fileName <- getLine
    writeFile fileName (encodeInputs playerBoard aiBoard difficulty aiBoardVisible aiNextMoves)
    putStrLn("The game has benn save in "++fileName)
    return ()

-- encodeInputs encodes the input into a list of strings
-- encodeInputs playerBoard aiBoard difficulty aiBoardVisible aiNextMoves
encodeInputs :: [[Int]] -> [[Int]] -> Int -> Bool -> [(Int,Int)] -> String
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
encodeBoard (h:t) = T.unpack ((T.intercalate (T.pack ",") (map T.pack (map show h)))) ++ "\n" ++ encodeBoard t


importBoard :: [[String]] -> [[Int]]
importBoard stringBoard = map importBoardHelper stringBoard

importBoardHelper :: [String] -> [Int]
importBoardHelper row = map toInt row

splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

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
        difficulty <- getDifficulty
        aiBoard <- setUpAIBoard
        let aiNextMoves = getMoves difficulty playerBoard
        play playerBoard aiBoard difficulty aiBoardVisible aiNextMoves
    else do -- load game
      putStrLn("What is your file's name?")
      fileName <- getLine
      file <- readFile fileName

      -- first line is [ai difficulty, is aiboard visible]
      -- second line is the ai's current list of next moves
      -- after that is 10 lines of AI board state and 10 lines of player board state
      let parsedFile = [splitsep (==',') line | line <- splitsep (=='\n') file]
      let difficulty = (toInt ((parsedFile !! 0) !! 0))
      let aiBoardVisible = (((parsedFile !! 0) !! 1) == "True")
      let aiNextMoves = (toCoord (map toInt (parsedFile !! 1)))
      let aiData = drop 2 . take 12 $ parsedFile
      let playerData = drop 12 . take 22 $ parsedFile
      let aiBoard = importBoard aiData
      let playerBoard = importBoard playerData
      play playerBoard aiBoard difficulty aiBoardVisible aiNextMoves












