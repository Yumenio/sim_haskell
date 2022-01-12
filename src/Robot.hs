module Robot where
import Utils (subNth0)

moveLeftR board i 0 = board
moveLeftR board i j = moveAny board i j 0 (-1) 'X'
  -- let board' = subNth0 board i j 'X' in subNth0 board' i (j-1) 'R'

moveAny board i j deltaI deltaJ sub = let oldItem = board!!i!!j; board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem in subNth0 board' i j sub