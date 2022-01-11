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
generateObstacles :: [[Char]] -> Int -> [[Char]]
generateObstacles board seed =
  let m = length board; n = length $ head board; perc = 10; amount = div (m*n*perc) 100 in generateObstaclesAux board amount seed

-- generateObstaclesAux :: [[Char]] -> Int -> [[Char]]
generateObstaclesAux :: [[Char]] -> Int -> Int -> [[Char]]
generateObstaclesAux board amount seed  | amount == 0 = board
                                        | otherwise = do
                                          let m = length board; n = length $ head board
                                          let x = runRandom rand seed; y = runRandom rand x; nboard = subNth0 board x y 'O' in generateObstaclesAux nboard (amount-1)
                                          -- let (head, row:rs) = splitAt x board; (rhead, _:rtail) = splitAt y row; newrow = rhead++['O']++rtail in generateObstaclesAux (head++[newrow]++rs) (amount-1)

rowLengthIO :: IO [[a]] -> IO Int 
rowLengthIO list = do
  fmap length list

colLengthIO :: IO [[a]] -> IO Int 
colLengthIO list = do
  escList <- list
  let row = head escList
  return $ length row


subNth0 :: [[Char]] -> Int -> Int -> Char -> [[Char]]
subNth0 board i j x = do
  let (head, row:rs) = splitAt i board; (rhead, _:rtail) = splitAt j row; newrow = rhead++[x]++rtail in head++[newrow]++rs

genBabyJail :: IO [[Char]] -> Int -> Int -> IO [[Char]]
genBabyJail board babyCount doneCount = if babyCount == doneCount then board else let (i,j) = getBoardIndex board doneCount; nboard = subNth0 board i j 'S' in genBabyJail nboard babyCount (doneCount+1)

getBoardIndex :: IO [[Char]] -> Int -> IO (Int, Int)
getBoardIndex board straightLineIndex = do
  let n = colLengthIO board;
  escN <- n
  let i = div straightLineIndex escN; j = rem straightLineIndex escN; in return (i,j)

rowDim :: (Foldable f, Num b) => f a -> b
rowDim = foldr (\ x -> (+) 1) 0

filterIntIO :: Int -> Int
filterIntIO x = x

-- filterBoardIO :: IO [[Char]] -> [[Char]]
filterBoardIO board = do
  escBoard <- board
  escBoard

pprint [] = putStrLn ""
pprint (r:t) = do
  print r
  pprint t

main :: IO ()
main = let m = 5; n = 5; x = 'X'; board = initBoard m n x; board' = generateObstacles board; board'' = genBabyJail board' 3 0 in pprint board''

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
