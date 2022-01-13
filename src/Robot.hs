module Robot where
import Utils (subNth0, getBoardIndex)

moveLeftR board i 0 = board
moveLeftR board i j = moveAny board i j 0 (-1) 'X'
  -- let board' = subNth0 board i j 'X' in subNth0 board' i (j-1) 'R'

moveRightR board i j =
  let
    m = length board
    n = length $ head board
    in
      if j == (m-1)
        then board
        else moveAny board i j 0 1 'X'

moveAny board i j deltaI deltaJ sub =
  let
    oldItem = board!!i!!j;
    board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem
    in subNth0 board' i j sub


moveRobots :: [[Char]] -> Int -> ([[Char]], Int)
moveRobots board seed =
  do
    let
      (i,j) = findRobot board 0;
      in
        if i == -1  --no robots left
          then (board, seed)
          else (moveLeftR board i j, seed)

findRobot :: [[Char]] -> Int -> (Int, Int)
findRobot board index =
  let
    m = length board;
    n = length $ head board;
    (i,j) = getBoardIndex board index;
    item = board!!i!!j
    in
      if index > (m-1)*(n-1)
        then (-1, -1)
        else
          if item == 'R'
            then (i,j)
            else findRobot board (index+1)