module Utils where
import Random (runRandom, rand)
import Data.List

fill x times  | times == 1 = [x]
              | times < 1 = []
              | otherwise = x : fill x (times-1)

initBoard m n x  | m == 1 = [fill x n]
                  | m < 1 =  []
                  | otherwise = fill x n : initBoard (m-1) n x

subNth0 board i j x = 
  let
    (h, row:rs) = splitAt i board;
    (rhead, _:rtail) = splitAt j row;
    newrow = rhead++[x]++rtail
    in h++[newrow]++rs

getBoardIndex :: [[Char]] -> Int -> (Int, Int)
getBoardIndex board straightLineIndex =
  let
    n = length $ head board;
    i = div straightLineIndex n;
    j = rem straightLineIndex n;
    in (i,j)

randomAdj :: [[Char]] -> Int -> Int -> Int -> (Int, Int, Int)
randomAdj board i j seed =
  let
    r = runRandom rand seed
    rMod = mod r 4
    in
      case rMod of
        0 -> if validPos board i (j+1) then (i, j+1, r) else randomAdj board i j r
        1 -> if validPos board (i+1) j then (i+1, j, r) else randomAdj board i j r
        2 -> if validPos board i (j-1) then (i, j-1, r) else randomAdj board i j r
        3 -> if validPos board (i-1) j then (i-1, j, r) else randomAdj board i j r
        _ -> (i, j, r) -- should not happen but who knows x)

validPos :: [[Char]] -> Int -> Int -> Bool
validPos board i j =
  let
    m = length board
    n = length $ head board
    in
      i >= 0 && i < m && j >= 0 && j < n

validPosMap :: ([[Char]], Int, Int) -> Bool
validPosMap (board, i, j) =
  validPos board i j

rowDim :: (Foldable f, Num b) => f a -> b
rowDim = foldr (\ x -> (+) 1) 0

pprint :: [[Char]] -> IO ()
pprint [] = putStrLn ""
pprint (r:t) = do
  print r
  pprint t


dirt :: [Char] -> Int
dirt [] = 0
dirt ('C':t) = 1 + dirt t
dirt (_:t) = 0 + dirt t

dirtPerc :: [[Char]] -> Int
dirtPerc board = let m = length board; n = length board in div (sum (map dirt board)*100) (m*n)

cleanPerc board = 100 - dirtPerc board


adjacents :: (Int, Int) -> (Int, Int) -> Bool
adjacents (i,j) (di, dj)  | i == di = j == (dj-1) || j == (dj+1)
                          | j == dj = i == (di-1) || i == (di+1)
                          | otherwise = False


getAdjacents :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getAdjacents board (i,j) visited =
  let
    candidates = [(board,i,j+1), (board,i,j-1), (board,i+1,j), (board,i-1,j)]
    validCandidates =  filter validPosMap candidates
    validCandidates' = map (\(_,i,j) -> (i,j)) validCandidates
    in
      validCandidates' \\ visited


  