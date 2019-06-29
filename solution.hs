import Data.List.Split

data Cell = X | O | D | T deriving (Show)

toCell :: Char -> Cell
toCell '.' = D
toCell 'X' = X
toCell 'O' = O
toCell 'T' = T

data Case = WX | WO | Draw | Unfinished

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
toCellList s = map toCell s

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

main = do
  ts  <- parseCases `fmap` getContents
  flip mapM_ (zip [1..] ts) $ \(i,t) -> do
  putStrLn $ "Case #" ++ show i ++ ": " ++ show t
  return ()
