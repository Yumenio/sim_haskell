module Robot where
import Utils (subNth0, getBoardIndex)

-- I, J, State
data Robot = Robot { robotRow :: Int, robotCol :: Int, robotState :: Int} deriving (Show)

robotAll :: Robot -> (Int, Int, Int)
robotAll robot = (robotRow robot, robotCol robot, robotState robot)

moveLeftR :: [[Char ]] -> Robot -> ([[Char]], Robot)
moveLeftR board robot =
  let
    (i,j,s) = robotAll robot
    in
      if j == 0
        then (board,robot)
        else
          let
            board' = moveAny board i j 0 (-1) 'X'
            robot' = Robot i (j-1) s
            in (board', robot')

  -- let board' = subNth0 board i j 'X' in subNth0 board' i (j-1) 'R'

moveUpR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveUpR board robot =
  let
    (i,j,s) = robotAll robot
    in
      if i == 0
        then (board,robot)
        else
          let
            board' = moveAny board i j (-1) 0 'X'
            robot' = Robot (i-1) j s
            in (board', robot')

moveRightR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveRightR board robot =
  let
    (i,j,s) = robotAll robot
    m = length board
    in
      if i == m
        then (board,robot)
        else
          let
            board' = moveAny board i j 0 1 'X'
            robot' = Robot i (j+1) s
            in (board', robot')

moveDownR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveDownR board robot =
  let
    (i,j,s) = robotAll robot
    m = length board
    in
      if i == (m-1)
        then (board,robot)
        else
          let
            board' = moveAny board i j 1 0 'X'
            robot' = Robot (i+1) j s
            in (board', robot')


-- cleanR board i j = 

moveAny board i j deltaI deltaJ sub =
  if canMove board (i+deltaI) (j+deltaJ)
    then
      let
        oldItem = board!!i!!j;
        board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem
        in subNth0 board' i j sub
    else
      board


canMove :: [[Char]] -> Int -> Int -> Bool
canMove board i j =
  let
    elem = board!!i!!j
    in  -- O => Obstacle, R => Robot, B => Baby in jail xd
      not (elem == 'O' || elem == 'R' || elem == 'B')

moveRobots :: [[Char]] -> [Robot] -> Int -> ([[Char]], [Robot], Int)
moveRobots board [] seed = (board, [], seed)
moveRobots board (robot: rs) seed =
  let
    (board', robot') = moveDownR board robot
    (board'', rs', seed') = moveRobots board' rs seed
    in (board'', robot':rs', seed')

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