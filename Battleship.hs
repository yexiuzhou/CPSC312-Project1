type Coordinate = (Int, Int)
type Ship = [Coordinate]
type Field = [[Bool]]

-- Probably also want a player type

-- Initialize the play field
-- LOL hexagonal field - gotta figure out how hashmaps or something work in Haskell
initField :: Field

-- Extract the coordinate from the string
convertInputToCoords :: String -> Coordinate

-- Split a string containing coordinates separated by semi-colons into a list of coordinates
splitCoordsInString :: String -> [String]

-- Check if a coordinate lies inside the field
validateCoords :: Coordinate -> Bool

-- Make sure that the ship is given valid coordinates (in a row/col, no overlap with existing ships)
-- ie for a ship 3 long, make sure it receives 3 coords that are in the same "row"
-- might want to have data about "rows" when we initialize the field
-- input: already placed ships, current ship (coords), length of current ship
validateShipCoordinates :: [Ship] -> Ship -> Int -> Bool

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

-- Input the names of the player (computer will always be "Computer")
inputNames :: IO [String]

-- The entry point of the program
main :: IO ()