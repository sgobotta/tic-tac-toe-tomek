import Data.List.Split
import System.Environment

data Cell = X | O | D | T deriving (Show, Eq)

toCell :: Char -> Cell
toCell '.' = D
toCell 'X' = X
toCell 'O' = O
toCell 'T' = T

data Case = WX | WO | Draw | Unfinished deriving (Show, Eq)

eval :: [Cell] -> Case
eval cs = if any (\c -> c == D) cs
          then Unfinished
          else if isWinner X cs
               then WX
               else if isWinner O cs
                    then WO
                    else Draw

evalCase :: [Case] -> Case
evalCase cs = if any (\c -> c == WX) cs
              then WX
              else if any (\c -> c == WO) cs
                   then WO
                   else if any (\c -> c == Unfinished) cs
                        then Unfinished
                        else Draw

answer :: Case -> String
answer WX         = "X won"
answer WO         = "O won"
answer Draw       = "Draw"
answer Unfinished = "Game has not completed"

isWinner :: Cell -> [Cell] -> Bool
isWinner c cs = all (\x -> x == c || x == T) cs

parseCases :: String -> [[[Cell]]]
parseCases x = let (n:ts) = filter (not . null) (lines x)
               in take (read n) (parseCase (chunksOf 4 ts))

parseCase :: [[String]] -> [[[Cell]]]
parseCase [] = []
parseCase (xs:xss) = (generateOdds xs) : parseCase xss

generateOdds :: [String] -> [[Cell]]
generateOdds ss = getRows ss
                  ++ getColumns ss ((length ss) - 1)
                  ++ getDiagonals ss ((length ss) - 1)

getRows :: [String] -> [[Cell]]
getRows [] = []
getRows (x:xs) = (toCellList x) : getRows xs

toCellList :: String -> [Cell]
toCellList = map toCell

getColumns :: [String] -> Int -> [[Cell]]
getColumns ts l = map (\n -> getColumn n ts) [0..l]

getColumn :: Int -> [String] -> [Cell]
getColumn n = map (toCell . (\s -> (!!) s n))

getDiagonals :: [String] -> Int -> [[Cell]]
getDiagonals xs l = let makeIndexOf = (\(ss,n) xs -> toCell (ss !! n) : xs)
                        foldTTTT    = foldr makeIndexOf []
                        is          = [0..l]
                        zipCases    = (\f -> zip (f xs) is)
                        getDiagonal = (\f -> foldTTTT (zipCases f))
                    in
                      (getDiagonal id) : (getDiagonal reverse) : []

showC :: (Int, String) -> String
showC (n, s) = "Case #" ++ show n ++ ": " ++ s

main = do
  args <- getArgs
  cases  <- parseCases `fmap` (readFile (head args)) -> do
  putStr (map show cases)
  return ()


  -- flip mapM_ (zip [1..] cases) $ \(i,t) -> do
  --   putStrLn $ "Case #" ++ show i ++ ": " ++ answer (evalCase (map eval t))

  -- main = do
  --   args <- getArgs
  --   cases  <- parseCases `fmap` (readFile (head args))
  --   sols <- map (\c -> answer (evalCase (map eval c))) cases
  --   writeFile (unlines map showC (zip [1..] sols)) "small"
  --   return ()
