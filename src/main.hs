{-# LANGUAGE FlexibleContexts #-}
import System.Random
import Distribution.Simple.Program.HcPkg (list)
import Random

toStr :: [Int] -> String
toStr [] = ""
toStr (h:t) =
  let xh = show h
  in xh ++ " " ++ toStr t

fill x times  | times == 1 = [x]
              | times < 1 = []
              | otherwise = x : fill x (times-1)

initBoard m n x  | m == 1 = [fill x n]
                  | m < 1 =  []
                  | otherwise = fill x n : initBoard (m-1) n x


-- generateObstacles :: [[Char]] -> [[Char]]
generateObstacles :: [[Char]] -> Int -> ([[Char]], Int)
generateObstacles board seed =
  let m = length board; n = length $ head board; perc = 10; amount = div (m*n*perc) 100 in generateObstaclesAux board amount seed

-- generateObstaclesAux :: [[Char]] -> Int -> [[Char]]
generateObstaclesAux :: [[Char]] -> Int -> Int -> ([[Char]], Int)
generateObstaclesAux board amount seed  | amount == 0 = (board, seed)
                                        | otherwise = let m = length board; n = length $ head board; x = runRandom rand seed; y = runRandom rand x; xMod = mod x m; yMod = mod y n; nboard = subNth0 board xMod yMod 'O' in generateObstaclesAux nboard (amount-1) y 
                                          -- let (head, row:rs) = splitAt x board; (rhead, _:rtail) = splitAt y row; newrow = rhead++['O']++rtail in generateObstaclesAux (head++[newrow]++rs) (amount-1)

rowLengthIO :: IO [[a]] -> IO Int 
rowLengthIO list = do
  fmap length list

colLengthIO :: IO [[a]] -> IO Int 
colLengthIO list = do
  escList <- list
  let row = head escList
  return $ length row


-- subNth0 :: [[Char]] -> Int -> Int -> Char -> [[Char]]
-- subNth0 [] i j x = []
-- subNth0 ([]:t) i j x = []
subNth0 board i j x = 
  let (h, row:rs) = splitAt i board; (rhead, _:rtail) = splitAt j row; newrow = rhead++[x]++rtail in h++[newrow]++rs


testFunc list i j x = let (h, row:rs) = splitAt i list; (rhead, _:rtail) = splitAt j row; newrow = rhead++[x]++rtail in h++[newrow]++rs

genBabyJail :: [[Char]] -> Int -> Int -> [[Char]]
genBabyJail board babyCount doneCount = if babyCount == doneCount then board else let (i,j) = getBoardIndex board doneCount; nboard = subNth0 board i j 'S' in genBabyJail nboard babyCount (doneCount+1)

getBoardIndex :: [[Char]] -> Int -> (Int, Int)
getBoardIndex board straightLineIndex =
  let n = length $ head board; i = div straightLineIndex n; j = rem straightLineIndex n; in (i,j)

rowDim :: (Foldable f, Num b) => f a -> b
rowDim = foldr (\ x -> (+) 1) 0

pprint :: [[Char]] -> IO ()
pprint [] = putStrLn ""
pprint (r:t) = do
  print r
  pprint t

simulate :: Int -> Int -> Int -> IO()
simulate m n seed = let x = 'X'; board = initBoard m n x; (board', seed') = generateObstacles board seed; board'' = genBabyJail board' 3 0 in pprint board''

cleanPercD board =
  let m = length board; n = length $ head board in cleanPercW board 0 0 0 (m-1) (n-1)

-- clean_perc_w :: [[]]
cleanPercW board x y dirty m n  | x == m && y == n && board!!x!!y == 'C' = div ((dirty+1)*100) ((m+1)*(n+1))
                                  | x == m && y == n = div (dirty*100) ((m+1)*(n+1))
                                  | y == n && board!!x!!y == 'C' = cleanPercW board (x+1) 0 (dirty+1) m n
                                  | board!!x!!y == 'C' = cleanPercW board x (y+1) (dirty+1) m n
                                  | y == n = cleanPercW board (x+1) 0 dirty m n
                                  | otherwise = cleanPercW board x (y+1) dirty m n


dirt :: [Char] -> Int
dirt [] = 0
dirt ('C':t) = 1 + dirt t
dirt (_:t) = 0 + dirt t

dirtPerc :: [[Char]] -> Int
dirtPerc board = let m = length board; n = length board in div (sum (map dirt board)*100) (m*n)

cleanPerc board = 100 - dirtPerc board

rndFilter :: Int -> Int
rndFilter x = x

rrr :: IO ()
rrr = do
  r <- randomRIO(1,100)
  print $ rndFilter r

bnr :: IO Int
bnr = let r = randomRIO(1,100) in r
