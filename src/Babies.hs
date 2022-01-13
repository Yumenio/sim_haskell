module Babies where
import Utils (subNth0, getBoardIndex)

data Baby = Baby {babyRow :: Int, babyCol :: Int} deriving (Show)

babyCoor :: Baby -> (Int, Int)
babyCoor baby = (babyRow baby, babyCol baby)


moveLeftB :: [[Char ]] -> Baby -> ([[Char]], Baby)
moveLeftB board baby =
  let
    (i,j) = babyCoor baby
    in
      if j == 0
        then (board,baby)
        else
          let
            board' = moveAnyB board i j 0 (-1) 'X'
            baby' = Baby i (j-1)
            in (board', baby')

  -- let board' = subNth0 board i j 'X' in subNth0 board' i (j-1) 'R'

moveUpB :: [[Char ]] -> Baby -> ([[Char]], Baby)
moveUpB board baby =
  let
    (i,j) = babyCoor baby
    in
      if j == 0
        then (board,baby)
        else
          let
            board' = moveAnyB board i j (-1) 0 'X'
            baby' = Baby (i-1) j
            in (board', baby')


moveRightB :: [[Char]] -> Baby -> ([[Char]], Baby)
moveRightB board baby =
  let
    (i,j) = babyCoor baby
    n = length $ head board
    in
      if i == (n-1)
        then (board,baby)
        else
          let
            board' = moveAnyB board i j 0 1 'X'
            baby' = Baby i (j+1)
            in (board', baby')


moveDownB :: [[Char]] -> Baby -> ([[Char]], Baby)
moveDownB board baby =
  let
    (i,j) = babyCoor baby
    m = length board
    in
      if i == m
        then (board,baby)
        else
          let
            board' = moveAnyB board i j 1 0 'X'
            baby' = Baby (i+1) j
            in (board', baby')


-- cleanR board i j = 

moveAnyB board i j deltaI deltaJ sub =
  if canMoveB board (i+deltaI) (j+deltaJ)
    then
      let
        oldItem = board!!i!!j;
        board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem
        in subNth0 board' i j sub
    else
      board


canMoveB :: [[Char]] -> Int -> Int -> Bool
canMoveB board i j =
  let
    elem = board!!i!!j
    in  -- O => Obstacle, R => Robot, B => Baby in jail xd
      not (elem == 'O' || elem == 'R' || elem == 'B')



genBabyJail :: [[Char]] -> Int -> Int -> [[Char]]
genBabyJail board babyCount doneCount =
  if babyCount == doneCount
    then board
    else
      let
        (i,j) = getBoardIndex board doneCount;
        nboard = subNth0 board i j 'S'
        in genBabyJail nboard babyCount (doneCount+1)
