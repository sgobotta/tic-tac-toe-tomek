import System.Environment

-- Cell represents a character ofa tic-tac-toe-tomek odd, where:
-- D: a blank space or dot (.)
-- X: an X player move
-- O: an O player move
-- T: the T symbol
data Cell = MkCell Cell | X | O | D | T deriving (Show, Eq)

-- Purpose: given a Char representing a move in a tic-tac-toe-tomek game,
-- returns it's equivalent in a Cell structure.
toCell :: Char -> Cell
toCell '.' = MkCell D
toCell 'X' = MkCell X
toCell 'O' = MkCell O
toCell 'T' = MkCell T

-- Case represents the state of a game, where:
-- WX: the winner is W
-- WO: the winner is O
-- Draw: a game finished without winners
-- Unfinished: the game did not finished
data Case = WX | WO | Draw | Unfinished deriving (Show, Eq)

-- A row is used to represent a row in a board, but also represents a column or a diagnoal of the board.
type Odd = [(Cell)]
type Row = [(Cell)]
type Column = [(Cell)]
type Diagonal = [(Cell)]

-- A board type is a list of all the rows, columns and diagonals in a given board case. Every odd.
type Board = [[(Cell)]]

-- Purpose: given a Board, representing every odd in a tic-tac-toe-tomek game,
-- returns a simplified result for a single tic-tac-toe-tomek case.
evalCaseChunks :: Board -> [Case]
evalCaseChunks css = map evalCaseChunk css

-- Purpose: given an Odd (Row or Column or Diagonal),  returns the state of that odd represented as a Case.
evalCaseChunk :: Odd -> Case
evalCaseChunk cs | any (\(MkCell c) -> c == D) cs = Unfinished
                 | isWinner X cs = WX
                 | isWinner O cs = WO
                 | otherwise = Draw

-- Purpose: given a Cell `c` (X or O), evaluates an Odd to determine if `c` is a winner.
isWinner :: Cell -> Odd -> Bool
isWinner c = all (\(MkCell x) -> x == c || x == T)

-- Purpose: given a list of Case, representing a bunch of odds for a single
-- tic-tac-toe-tomek game, returns a final state for that game, represented as
-- a Case.
evalCases :: [Case] -> Case
evalCases cs | any (\c -> c == WX) cs = WX
             | any (\c -> c == WO) cs = WO
             | any (\c -> c == Unfinished) cs = Unfinished
             | otherwise = Draw

-- Purpose: given a Case, returns a String representing a game result.
answer :: Case -> String
answer WX         = "X won"
answer WO         = "O won"
answer Draw       = "Draw"
answer Unfinished = "Game has not completed"


-- Purpose: given a file content with `n` cases, returns a list of `n` Boards,
-- where each board is represented as a list of rows. Each of these rows
-- represents an odd for a tic-tac-toe-tomek row, column and diagonal odds.
--
-- Precondition: in the file content input, each case must be separated with a
-- line break.
parseContent :: String -> [Board]
parseContent x = let (n:ts) = filter (not . null) (lines x)
               in
                 take (read n) (parseCases (chunksOf 4 ts))

-- Purpose: given an int and a list of String, returns a tuple where it's first
-- component is a chunked list and the second component the rest of the list
takeN :: Int -> [String] -> ([String], [String])
takeN n xs = (take n xs, drop n xs)

-- Purpose: given an int and a list of string, returns a list, each with
-- length of `n`
chunksOf :: Int -> [String] -> [[String]]
chunksOf n xs = let chunks = takeN n xs
                 in
                   (fst chunks) : chunksOf n (snd chunks)

-- Purpose: given a case represented as a list of String, returns all possible
-- odds in a tic-tac-toe-tomek game where each character is represented as a
-- Cell.
parseCases :: [[String]] -> [Board]
parseCases []       = []
parseCases (xs:xss) = (generateOdds xs) : parseCases xss

-- Purpose: given a list of String representing tic-tac-toe-tomek rows,
-- generates every odd of a game to return a board, representing
-- every possible row, column and diagonal.
generateOdds :: [String] -> [Odd]
generateOdds ss = getRows ss
                  ++ getColumns ss ((length ss) - 1)
                  ++ getDiagonals ss

-- Purpose: given a list of String representing tic-tac-toe-tomek rows, returns
-- a tic-tac-toe-tomek game represented as a board of rows.
getRows :: [String] -> [Row]
getRows []     = []
getRows (x:xs) = (getRow x) : getRows xs

-- Purpose: given a String representing an odd of a tic-tac-toe-tomek game,
-- returns a row.
getRow :: String -> Row
getRow = map toCell

-- Purpose: given a list of String representing tic-tac-toe-tomek rows, and an
-- Int representing the length of a game row, returns a list of every game columns
getColumns :: [String] -> Int -> [Column]
getColumns ts l = map (\n -> getColumn n ts) [0..l]

-- Purpose: given an Int representing the index of a game row, and a game row,
-- returns a specific game column
getColumn :: Int -> [String] -> Column
getColumn n = map (toCell . (\s -> (!!) s n))

-- Purpose: given a list of String representing a game row, and an Int
-- representing the length of a game diagonal, returns the two diagonals of a
-- tic-tac-toe-tomek game
getDiagonals :: [String] -> [Diagonal]
getDiagonals xs = let getDiagonal = (\xs -> (snd (foldr (\x (m, n) -> (m + 1, toCell ((!!) x m) : n)) (0, []) xs)))
                  in
                    (getDiagonal xs) : (getDiagonal (reverse xs)) : []

-- Purpose: given a tuple of Int and String, representing a game result, returns
-- a parsed String representing the case number with a game result.
showCase :: (Int, String) -> String
showCase (n, s) = "Case #" ++ show n ++ ": " ++ s

-- Purpose: given a file name and an output name, parses the input file content
-- and answers every case to write an output file containing the result for
-- each case.
main = do
  [input, output] <- getArgs
  cases           <- parseContent `fmap` (readFile input)
  writeFile output $
    unlines $
    map showCase $ zip [1..] $
    map (answer . evalCases . evalCaseChunks) cases
  return ()
