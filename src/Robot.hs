module Robot where
import Random
import Utils (subNth0, getBoardIndex, adjacents, getAdjacents)
import Babies
import Data.List

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
            board' = moveAnyR board i j 0 (-1) 'X'
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
            board' = moveAnyR board i j (-1) 0 'X'
            robot' = Robot (i-1) j s
            in (board', robot')

moveRightR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveRightR board robot =
  let
    (i,j,s) = robotAll robot
    n = length $ head board
    in
      if j == (n-1)
        then (board,robot)
        else
          let
            board' = moveAnyR board i j 0 1 'X'
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
            board' = moveAnyR board i j 1 0 'X'
            robot' = Robot (i+1) j s
            in (board', robot')


-- cleanR board i j = 

moveAnyR board i j deltaI deltaJ sub =
  if canMoveR board (i+deltaI) (j+deltaJ)
    then
      let
        oldItem = board!!i!!j;
        board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem
        in subNth0 board' i j sub
    else
      board


canMoveR :: [[Char]] -> Int -> Int -> Bool
canMoveR board i j =
  let
    elem = board!!i!!j
    in  -- O => Obstacle, C => Crap, R => Robot, B => Baby, Z => Baby in jail xd
      not (elem == 'O' || elem == 'C' || elem == 'R' || elem == 'B' || elem == 'Z')


moveRobots :: [[Char]] -> [Robot] -> Int -> ([[Char]], [Robot], Int)
moveRobots board [] seed = (board, [], seed)
moveRobots board (robot: rs) seed =
  let
    (board', robot') = moveRightR board robot
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




generateRobots :: [[Char]] -> Int -> Int -> ([[Char]], [Robot], Int)
generateRobots board amount seed = generateRobotsAux board amount seed []

generateRobotsAux :: [[Char]] -> Int -> Int -> [Robot] -> ([[Char]], [Robot], Int)
generateRobotsAux board 0 seed robots = (board, robots, seed)
generateRobotsAux board amount seed robots =
  let
    m = length board;
    n = length $ head board;
    x = runRandom rand seed; xMod = mod x m;
    y = runRandom rand x; yMod = mod y n
    in
      if board!!xMod!!yMod /= 'X'
        then generateRobotsAux board amount y robots
        else
          let
            board' = subNth0 board xMod yMod 'R'
            robot = Robot xMod yMod 1
            in generateRobotsAux board' (amount-1) y (robot:robots)


reactiveAgent :: [[Char]] -> Robot -> [Baby] -> ([[Char]], Robot, [Baby])
reactiveAgent board robot babies =
  let
    (i, j, s) = robotAll robot
    in
      case s of
        1 ->
          let
            _:path = lookForObjectiveR board robot
            l = length path
            (di, dj) = last path
            (board', robot') = followPath board robot path 2
            in
              if l == 2 && board!!di!!dj=='B'
                then
                  let
                    robot'' = Robot di dj 2
                    nbaby = Baby di dj 2
                    carriedBaby = findBabyAt (di, dj) babies
                    babies' = delete carriedBaby babies
                    babies'' = nbaby:babies'
                    in
                      (board', robot'', babies'')
                else
                  (board', robot', babies)
        2 ->
          let
            _:path = lookForBabyJail board robot
            l = length path
            in
              if l == 2
                then
                  let
                    dest = last path
                    (board', robot', babies') = depositBaby board robot babies dest
                    in (board', robot', babies') --mejorable
                else
                  let
                    src = head path
                    (desti, destj) = path!!1
                    carriedBaby = findBabyAt (desti, destj) babies
                    babies' = delete carriedBaby babies
                    nbaby = Baby desti destj 2
                    babies'' = nbaby:babies'
                    (board', robot') = followPath board robot path 1
                  in (board', robot', babies'')
        _ -> (board, robot, babies)


followPath :: [[Char]] -> Robot -> [(Int,Int)] -> Int -> ([[Char]], Robot)
followPath board robot path 0 = (board, robot)
followPath board robot [] steps = (board, robot)
followPath board robot (nextStep:tail) steps =
  let
    (i, j, s) = robotAll robot
    (di, dj) = nextStep
    in
      if adjacents (i,j) (di, dj)
        then
          let
            robot' = Robot di dj s
            board' = subNth0 board di dj 'R'
            board'' = subNth0 board' i j 'X'
            in
              followPath board'' robot' tail (steps-1)
        else
          error "invalid path"

depositBaby :: [[Char]] -> Robot -> [Baby] -> (Int, Int) -> ([[Char]], Robot, [Baby])
depositBaby board robot babies (di, dj)=
  let
    (i,j,_) = robotAll robot
    carriedBaby = findBabyAt (i,j) babies
    babies' = delete carriedBaby babies
    board' = subNth0 board di dj 'Z'
    robot' = Robot i j 1
    in
      (board', robot', babies')



findBabyAt :: (Int, Int) -> [Baby] -> Baby
findBabyAt (i, j) [] = error "No baby is being carried"
findBabyAt (i, j) (baby:btail) =
  let
    (bi, bj) = babyCoor baby
    in
      if i==bi && j==bj
        then
          baby
        else
          findBabyAt (i,j) btail

lookForObjectiveR :: [[Char]] -> Robot -> [(Int, Int)]
lookForObjectiveR board robot =
  let
    objectives = ['B', 'C']
    (i,j,s) = robotAll robot
    in
      bfsObjR board objectives (i,j)

lookForBabyJail :: [[Char]] -> Robot -> [(Int, Int)]
lookForBabyJail board robot =
  let
    (i,j,s) = robotAll robot
    in
      bfsGenericObj board [[(i,j)]] [(i,j)] ['S']

bfsObjR :: [[Char]] -> [Char] -> (Int, Int) -> [(Int, Int)]
bfsObjR board  objectives (i,j) =
  bfsGenericObj board [[(i,j)]] [(i,j)] objectives

bfsGenericObj :: [[Char]] -> [[(Int, Int)]] -> [(Int, Int)] -> [Char] -> [(Int, Int)]
bfsGenericObj board [] visited objectives = []
bfsGenericObj board queue visited objectives =
  do
    let
      (h:tail) = queue
      (i,j) = last h

    if (board!!i!!j) `elem` objectives
      then
        do
          h
      else
        let
          adjs = getAdjacents board (i,j) visited
          visited' = ((i,j):visited)
          in
            case adjs of
              [adj1] -> let
                newQueue = appendPath tail h [adj1]
                in
                  bfsGenericObj board  newQueue visited' objectives

              [adj1,adj2] -> let
                newQueue = appendPath tail h [adj1,adj2]
                in
                  bfsGenericObj board  newQueue visited' objectives

              [adj1,adj2,adj3] -> let
                newQueue = appendPath tail h [adj1,adj2,adj3]
                in
                  bfsGenericObj board  newQueue visited' objectives

              [adj1,adj2,adj3,adj4] -> let
                newQueue = appendPath tail h [adj1,adj2,adj3,adj4]
                in
                  bfsGenericObj board  newQueue visited' objectives


              _ -> bfsGenericObj board tail visited' objectives


appendPath :: [[(Int, Int)]] -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]                
appendPath queue path [] = queue
appendPath queue path (newItemForPath:rest) =
  let
    newPath = path ++ [newItemForPath]
    queue' = queue ++ [newPath]
    in
      appendPath queue' path rest





bfsGenericObjIO :: [[Char]] -> [[(Int, Int)]] -> [(Int, Int)] -> [Char] -> IO [(Int, Int)]
bfsGenericObjIO board [] visited objectives = do
  print "No path found"
  return []
bfsGenericObjIO board queue visited objectives =
  do
    let
      (h:tail) = queue
      (i,j) = last h

    print "actual queue ="
    print queue
    print "Analizing head:"
    print h
    if (board!!i!!j) `elem` objectives
      then
        do
          print "Found!!"
          return h
      else
        -- if length visited >= (length board * length (head board))
        if False
          then
            error "objective not found"
          else
            let
              adjs = getAdjacents board (i,j) visited
              visited' = ((i,j):visited)
              in
                case adjs of
                  [adj1] -> let
                    newQueue = appendPath tail h [adj1]
                    in
                      do
                        print "new queue="
                        pprintQueue newQueue
                        bfsGenericObjIO board  newQueue visited' objectives

                  [adj1,adj2] -> let
                    newQueue = appendPath tail h [adj1,adj2]
                    in
                      do
                        print "new queue="
                        pprintQueue newQueue
                        bfsGenericObjIO board  newQueue visited' objectives

                  [adj1,adj2,adj3] -> let
                    newQueue = appendPath tail h [adj1,adj2,adj3]
                    in
                      do
                        print "new queue="
                        pprintQueue newQueue
                        bfsGenericObjIO board  newQueue visited' objectives

                  [adj1,adj2,adj3,adj4] -> let
                    newQueue = appendPath tail h [adj1,adj2,adj3,adj4]
                    in
                      do
                        print "new queue="
                        pprintQueue newQueue
                        bfsGenericObjIO board  newQueue visited' objectives


                  _ -> bfsGenericObjIO board tail visited' objectives

pprintQueue :: [[(Int, Int)]] -> IO ()
pprintQueue [] = return ()
pprintQueue (path:queueTail) =
  do
    print path
    pprintQueue queueTail