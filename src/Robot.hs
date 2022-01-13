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

moveUpR board 0 j = board
moveUpR board i j = moveAny board i j (-1) 0 'X'

moveRightR board i j =
  let
    m = length board
    -- n = length $ head board
    in
      if j == (m-1)
        then board
        else moveAny board i j 0 1 'X'

moveDownR board i j =
  let
    -- m = length board
    n = length $ head board
    in
      if i == (n-1)
        then board
        else moveAny board i j 1 0 'X'


-- cleanR board i j = 

moveAny board i j deltaI deltaJ sub =
  let
    oldItem = board!!i!!j;
    board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem
    in subNth0 board' i j sub


moveRobots :: [[Char]] -> [Robot] -> Int -> ([[Char]], [Robot], Int)
moveRobots board [] seed = (board, [], seed)
moveRobots board (robot: rs) seed =
  let
    (board', robot') = moveLeftR board robot
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