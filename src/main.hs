toStr :: [Int] -> String
toStr [] = ""
toStr (h:t) =
  let xh = show h
  in xh ++ " " ++ toStr t

fill x times  | times == 1 = [x]
              | times < 1 = []
              | otherwise = [x] ++ fill x (times-1)

init_table m n x  | m == 1 = fill x n
                  | m < 1 =  []
                  | otherwise = fill x n ++ init_table (m-1) n x