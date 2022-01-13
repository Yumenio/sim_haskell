module Utils where
import Random (runRandom, rand)

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

randomAdj :: Int -> Int -> Int -> (Int, Int, Int)
randomAdj i j seed =
  let
    r = runRandom rand seed
    rMod = mod r 4
    in
      case rMod of
        0 -> (i, j+1, r)
        1 -> (i+1, j, r)
        2 -> (i, j-1, r)
        3 -> (i-1, j, r)
        _ -> (i, j, r) -- should not happen but who knows x)


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