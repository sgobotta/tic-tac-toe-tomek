import Data.List.Split
import System.Environment

-- Cell represents a character ofa tic-tac-toe-tomek odd, where:
-- D: a blank space or dot (.)
-- X: an X player move
-- O: an O player move
-- T: the T symbol
data Cell = X | O | D | T deriving (Show, Eq)

-- Purpose: given a Char representing a move in a tic-tac-toe-tomek game,
-- returns it's equivalent in a Cell structure.
toCell :: Char -> Cell
toCell '.' = D
toCell 'X' = X
toCell 'O' = O
toCell 'T' = T

-- Case represents the state of a game, where:
-- WX: the winner is W
-- WO: the winner is O
-- Draw: a game finished without winners
-- Unfinished: the game did not finished
data Case = WX | WO | Draw | Unfinished deriving (Show, Eq)

-- Purpose: given a list of lists of Cell, representing every odd in a
-- tic-tac-toe-tomek game, returns a simplified result for a single
-- tic-tac-toe-tomek case.
evalCaseChunks :: [[Cell]] -> [Case]
evalCaseChunks css = map evalCaseChunk css

-- Purpose: given a list of Cell, representing an odd of a tic-tac-toe-tomek
-- game, returns the state of that odd represented as a Case.
evalCaseChunk :: [Cell] -> Case
evalCaseChunk cs = if any (\c -> c == D) cs
                   then Unfinished
                   else if isWinner X cs
                        then WX
                        else if isWinner O cs
                             then WO
                             else Draw

-- Purpose: given a Cell `c` (X or O), evaluates a list of Cell to determine
-- if `c` is a winner.
isWinner :: Cell -> [Cell] -> Bool
isWinner c = all (\x -> x == c || x == T)

-- Purpose: given a list of Case, representing a bunch of odds for a single
-- tic-tac-toe-tomek game, returns a final state for that game, represented as
-- a Case.
evalCases :: [Case] -> Case
evalCases cs = if any (\c -> c == WX) cs
               then WX
               else if any (\c -> c == WO) cs
                    then WO
                    else if any (\c -> c == Unfinished) cs
                         then Unfinished
                         else Draw
-- Purpose: given a Case, returns a String representing a game result.
answer :: Case -> String
answer WX         = "X won"
answer WO         = "O won"
answer Draw       = "Draw"
answer Unfinished = "Game has not completed"


-- Purpose: given a file content with `n` cases, returns a list of `n` cases
-- where each case is represented as a list of Cell lists. Each list of Cell
-- represents an odd for a tic-tac-toe-tomek row, column and diagonal odds.
--
-- Precondition: in the file content input, each case must be separated with a
-- line break.
parseCases :: String -> [[[Cell]]]
parseCases x = let (n:ts) = filter (not . null) (lines x)
               in
                 take (read n) (parseCase (chunksOf 4 ts))

-- Purpose: given a case represented as a list of String, returns all possible
-- odds in a tic-tac-toe-tomek game where each character is represented as a
-- Cell.
parseCase :: [[String]] -> [[[Cell]]]
parseCase []       = []
parseCase (xs:xss) = (generateOdds xs) : parseCase xss

-- Purpose: given a list of String representing tic-tac-toe-tomek rows,
-- generates every odd of a game to return a list of lists of Cell, representing
-- every possible row, column and diagonal.
generateOdds :: [String] -> [[Cell]]
generateOdds ss = getRows ss
                  ++ getColumns ss ((length ss) - 1)
                  ++ getDiagonals ss ((length ss) - 1)

-- Purpose: given a list of String representing tic-tac-toe-tomek rows, returns
-- a tic-tac-toe-tomek game represented as rows of Cell.
getRows :: [String] -> [[Cell]]
getRows []     = []
getRows (x:xs) = (toCellList x) : getRows xs

-- Purpose: given a String representing an odd of a tic-tac-toe-tomek game,
-- returns a list of converted characters to Cell.
toCellList :: String -> [Cell]
toCellList = map toCell

-- Purpose: given a list of String representing tic-tac-toe-tomek rows, and an
-- Int representing the length of a game row, returns a list of lists of Cell
-- representing a game columns.
getColumns :: [String] -> Int -> [[Cell]]
getColumns ts l = map (\n -> getColumn n ts) [0..l]

-- Purpose: given an Int representing the index of a game row, and a game row,
-- returns a list of Cell representing a game column.
getColumn :: Int -> [String] -> [Cell]
getColumn n = map (toCell . (\s -> (!!) s n))

-- Purpose: given a list of String representing a game row, and an Int
-- representing the length of a game column, returns the two columns of a
-- tic-tac-toe-tomek game.
getDiagonals :: [String] -> Int -> [[Cell]]
getDiagonals xs l = let makeIndexOf = (\(ss,n) xs -> toCell (ss !! n) : xs)
                        foldTTTT    = foldr makeIndexOf []
                        indexes     = [0..l]
                        zipCases    = (\f -> zip (f xs) indexes)
                        getDiagonal = (\f -> foldTTTT (zipCases f))
                    in
                      (getDiagonal id) : (getDiagonal reverse) : []

-- Purpose: given a tuple of Int and String, representing a game result, returns
-- a parsed String representing the case number with a game result.
showCase :: (Int, String) -> String
showCase (n, s) = "Case #" ++ show n ++ ": " ++ s

-- Purpose: given a file name and an output name, parses the input file content
-- and answers every case to write an output file containing the result for
-- each case.
main = do
  [input, output] <- getArgs
  cases           <- parseCases `fmap` (readFile input)
  writeFile output $
    unlines $
    map showCase $ zip [1..] $
    map (answer . evalCases . evalCaseChunks) cases
  return ()
