{-# LANGUAGE FlexibleContexts #-}
import System.Random

toStr :: [Int] -> String
toStr [] = ""
toStr (h:t) =
  let xh = show h
  in xh ++ " " ++ toStr t

fill x times  | times == 1 = [x]
              | times < 1 = []
              | otherwise = [x] ++ fill x (times-1)

init_board m n x  | m == 1 = [fill x n]
                  | m < 1 =  []
                  | otherwise = [fill x n] ++ init_board (m-1) n x


pprint [] = putStrLn ""
pprint (r:t) = do
  print r
  pprint t

main :: IO ()
main = let m = 3; n = 3; x = 'X'; board = init_board m n x in pprint board

clean_perc_d board =
  let m = length board; n = length $ board!!0 in clean_perc_w board 0 0 0 (m-1) (n-1)

-- clean_perc_w :: [[]]
clean_perc_w board x y dirty m n  | x == m && y == n && board!!x!!y == 'C' = div ((dirty+1)*100) ((m+1)*(n+1))
                                  | x == m && y == n = div (dirty*100) ((m+1)*(n+1))
                                  | y == n && board!!x!!y == 'C' = clean_perc_w board (x+1) 0 (dirty+1) m n
                                  | board!!x!!y == 'C' = clean_perc_w board x (y+1) (dirty+1) m n
                                  | y == n = clean_perc_w board (x+1) 0 dirty m n
                                  | otherwise = clean_perc_w board x (y+1) dirty m n


dirt :: [Char] -> Int
dirt [] = 0
dirt ('C':t) = 1 + dirt t
dirt (_:t) = 0 + dirt t

dirt_perc :: [[Char]] -> Int
dirt_perc board = let m = length board; n = length board in div ((sum $ map dirt board)*100) (m*n)

clean_perc board = 100 - (dirt_perc board)

rnd_filter :: Int -> Int
rnd_filter x = x

rrr :: IO ()
rrr = do
  r <- randomRIO(1,100)
  print $ rnd_filter r

bnr :: IO Int
bnr = let r = randomRIO(1,100) in r
