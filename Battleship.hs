type Coordinate = (Char, Int)
type Ship = [Coordinate]
type Field = [[Bool]]

validX = ['A' .. 'J']
validY = [1 .. 10]


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
    if isShipVisibile
      then do
        -- TODO display everything properly

    else do
      -- TODO replace ships with blanks (unless theyre hit)
      

{------------------------------- Helper Functions -------------------------------------------}

-- setUpPlayerBoard
-- toInt
-- setUpAIBoard

-- ai function should take in a list of nextMoves and return a new list of next moves based on difficulty
-- eg: ai difficulty nextMoves

-- getMoves
-- toCoord
-- importBoard


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
      aiTarget <- -- TODO
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
          play newPlayerBoard newAIBoard ai aiBoardVisible aiNextMoves difficulty
 


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
encodeInputs :: [[Int]] -> [[Int]] -> Int -> Bool -> [Int] -> [[String]]

encodeInputs = 


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
      		aiNextMoves <- (toCoord (toInt (parsedFile !! 1)))
      		aiData <- slice 2 10 parsedFile
      		playerData <- slice 12 10 parsedFile
      		aiBoard <- importBoard aiData
      		playerBoard <- importBoard playerData
      		play playerBoard aiBoard difficulty aiBoardVisible aiNextMoves












