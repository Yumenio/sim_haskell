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
main = let m = 3; n = 3; x = "0"; board = init_board m n x in print board