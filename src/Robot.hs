module Robot where
import Random
import Utils (subNth0, getBoardIndex, adjacents, getAdjacents)
import Babies
import Data.List

-- I, J, State, Beneath(the char that is beneath him in the board)
data Robot = Robot { robotRow :: Int, robotCol :: Int, robotState :: Int, robotBth :: Char} deriving (Show)

robotAll :: Robot -> (Int, Int, Int)
robotAll robot = (robotRow robot, robotCol robot, robotState robot)

moveLeftR :: [[Char ]] -> Robot -> ([[Char]], Robot)
moveLeftR board robot =
  let
    (i,j,s) = robotAll robot
    beneath = robotBth robot
    in
      if j == 0
        then (board,robot)
        else
          let
            board' = moveAnyR board i j 0 (-1) beneath
            nbeneath = board!!i!!(j-1)
            robot' = Robot i (j-1) s nbeneath
            in (board', robot')

  -- let board' = subNth0 board i j 'X' in subNth0 board' i (j-1) 'R'

moveUpR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveUpR board robot =
  let
    (i,j,s) = robotAll robot
    beneath = robotBth robot
    in
      if i == 0
        then (board,robot)
        else
          let
            board' = moveAnyR board i j (-1) 0 beneath
            nbeneath = board!!(i-1)!!j
            robot' = Robot (i-1) j s nbeneath
            in (board', robot')

moveRightR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveRightR board robot =
  let
    (i,j,s) = robotAll robot
    beneath = robotBth robot
    n = length $ head board
    in
      if j == (n-1)
        then (board,robot)
        else
          let
            board' = moveAnyR board i j 0 1 'X'
            nbeneath = board!!i!!(j+1)
            robot' = Robot i (j+1) s nbeneath
            in (board', robot')

moveDownR :: [[Char]] -> Robot -> ([[Char]], Robot)
moveDownR board robot =
  let
    (i,j,s) = robotAll robot
    beneath = robotBth robot
    m = length board
    in
      if i == (m-1)
        then (board,robot)
        else
          let
            board' = moveAnyR board i j 1 0 'X'
            nbeneath = board!!(i+1)!!j
            robot' = Robot (i+1) j s nbeneath
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
            robot = Robot xMod yMod 1 'X'
            in generateRobotsAux board' (amount-1) y (robot:robots)


reactiveAgent :: [[Char]] -> Robot -> [Baby] -> ([[Char]], Robot, [Baby])
reactiveAgent board robot babies =
  let
    (i, j, s) = robotAll robot
    beneath = robotBth robot
    in
      if beneath == 'C'
        then let
          robot' = Robot i j s 'X'
          in
            (board, robot', babies)
        else

      case s of
        -- looking for nearest objective
        1 ->
          let
            objPath = lookForObjectiveR board robot
            in
              if null objPath
                then
                  (board, robot, babies)
                else
                  let
                    _:path = objPath
                    l = length path
                    (di, dj) = last path
                    (board', robot') = followPath board robot path 1
                    in
                      if l <= 2 && board!!di!!dj=='B'
                        then
                          let
                            robot'' = Robot di dj 2 'B'
                            nbaby = Baby di dj 2
                            carriedBaby = findBabyAt (di, dj) babies
                            babies' = delete carriedBaby babies
                            babies'' = nbaby:babies'
                            in
                              (board', robot'', babies'')
                        else
                          (board', robot', babies)

        --carrying a baby
        2 ->
          let
            objPath = lookForBabyJail board robot
            in
              if null objPath
                then --no babyJail cell accessible atm
                  (board, robot, babies)
                else
                  let
                    src:path = objPath
                    l = length path
                    in
                      if l == 1
                        then
                          let
                            dest = last path
                            (board', robot', babies') = depositBaby board robot babies dest
                            in (board', robot', babies') --mejorable
                        else
                          let
                            (srci, srcj) = src
                            (desti, destj) = head path
                            nbaby = Baby desti destj 2
                            carriedBaby = findBabyAt (srci, srcj) babies
                            babies' = delete carriedBaby babies
                            babies'' = nbaby:babies'
                            (board', robot') = followPath board robot path 2
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
            beneath = robotBth robot
            nbeneath = board!!di!!dj
            robot' = Robot di dj s nbeneath
            board' = subNth0 board di dj 'R'
            board'' = subNth0 board' i j beneath
            in
              followPath board'' robot' tail (steps-1)
        else
          error "invalid path"

depositBaby :: [[Char]] -> Robot -> [Baby] -> (Int, Int) -> ([[Char]], Robot, [Baby])
depositBaby board robot babies (di, dj)=
  let
    (i,j,_) = robotAll robot
    beneath = robotBth robot
    carriedBaby = findBabyAt (i,j) babies
    babies' = delete carriedBaby babies
    board' = subNth0 board di dj 'Z'
    robot' = Robot i j 1 beneath
    in
      (board', robot', babies')



findBabyAt :: (Int, Int) -> [Baby] -> Baby
findBabyAt (i, j) [] = error $ show (i,j)
findBabyAt (i, j) (baby:btail) =
  let
    (bi, bj) = babyCoor baby
    in
      if i==bi && j==bj
        then
          baby
        else
          findBabyAt (i,j) btail

jailReachable :: [[Char]] -> Robot -> Bool
jailReachable board robot =
  let
    path = lookForBabyJail board robot
    in
      case path of
        [] -> False
        _  -> True

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
      bfsGenericObjNoBaby board [[(i,j)]] [(i,j)] ['S']

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
          adjs = getAdjacents board (i,j) visited ['O','Z']
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


bfsGenericObjNoBaby :: [[Char]] -> [[(Int, Int)]] -> [(Int, Int)] -> [Char] -> [(Int, Int)]
bfsGenericObjNoBaby board [] visited objectives = []
bfsGenericObjNoBaby board queue visited objectives =
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
          adjs = getAdjacents board (i,j) visited ['O', 'Z', 'B']
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


modelBasedAgent :: [[Char]] -> Robot -> [Baby] -> ([[Char]], Robot, [Baby])
modelBasedAgent board robot babies =
  do
    let
      (i,j,s) = robotAll robot
      jail_reachable = jailReachable board robot
      beneath = robotBth robot
      in
        if beneath == 'C'
          then let
            robot' = Robot i j s 'X'
            in
              (board, robot', babies)
          else
      
        case s of
          --looking for the best path to follow
          1 ->
            --if there are no babies, then a good option is to just find the nearest C, clean it, and repeat
            --note that finding the path containing more C on it is irrelevant, since the job will be done when ALL C are cleaned, and a plain bfs is MUCH MORE efficient
            if null babies
              then
                let
                  objPath = lookForObjectiveR board robot
                  in
                    if null objPath
                      then (board, robot, babies)
                      else let
                        _:path = objPath
                        (board', robot') = followPath board robot path 1
                        in
                          (board', robot', babies)
              else
                if jail_reachable
                  then let
                    (opt_path, value) = dfsOptimalPath (board, (i,j), [], ([],0), ([],-10000))
                    in
                      if null opt_path
                        then (board, robot, babies)
                        else let
                          src:path = opt_path
                          (di, dj) = head path
                          (board', robot') = followPath board robot path 1
                          in
                            if board!!di!!dj == 'B'
                              then
                                let
                                  robot'' = Robot di dj 2 'X'
                                  nbaby = Baby di dj 2
                                  carriedBaby = findBabyAt (di, dj) babies
                                  babies' = delete carriedBaby babies
                                  babies'' = nbaby:babies'
                                  in
                                    (board', robot'', babies'')
                              else
                                (board', robot', babies)
                  else let --dfs ignoring the babies, since the jail is no longer accesible
                    (_:path, value) = dfsOptimalPathNoBaby (board, (i,j), [], ([],0), ([],-10000))
                    (board', robot') = followPath board robot path 1
                    in (board', robot', babies)
          

          2 ->
            if jail_reachable
              then let
                objPath = lookForBabyJail board robot
                in
                  if null objPath
                    then
                      (board, robot, babies)
                    else
                      let
                        src:path = objPath
                        l = length path
                        in
                          if l == 1
                            then
                              let
                                dest = last path
                                (board', robot', babies') = depositBaby board robot babies dest
                                in (board', robot', babies') --mejorable
                            else
                              let
                                (srci, srcj) = src
                                (desti, destj) = head path
                                nbaby = Baby desti destj 2
                                carriedBaby = findBabyAt (srci, srcj) babies
                                babies' = delete carriedBaby babies
                                babies'' = nbaby:babies'
                                (board', robot') = followPath board robot path 1
                              in (board', robot', babies'')
              else  -- acá tocaría soltar rápido al bebé de ser posible, pero kepereza
                (board, robot, babies)
          _ -> (board, robot, babies)


modelBasedAgentIO :: [[Char]] -> Robot -> [Baby] -> IO ([[Char]], Robot, [Baby])
modelBasedAgentIO board robot babies =
  do
    let
      (i,j,s) = robotAll robot
      jail_reachable = jailReachable board robot
      in
        case s of
          --looking for the best path to follow
          1 ->
            --if there are no babies, then a good option is to just find the nearest C, clean it, and repeat
            --note that finding the path containing more C on it is irrelevant, since the job will be done when ALL C are cleaned, and a plain bfs is MUCH MORE efficient
            if null babies
              then
                let
                  objPath = lookForObjectiveR board robot
                  in
                    if null objPath
                      then return (board, robot, babies)
                      else let
                        _:path = objPath
                        (board', robot') = followPath board robot path 1
                        in
                          return (board', robot', babies)
              else
                if jail_reachable
                  then let
                    (opt_path, value) = dfsOptimalPath (board, (i,j), [], ([],0), ([],-10000))
                    in
                      if null opt_path
                        then return (board, robot, babies)
                        else let
                          src:path = opt_path
                          -- l = length path
                          (di, dj) = head path
                          (board', robot') = followPath board robot path 2
                          in
                            if board!!di!!dj == 'B'
                              then
                                let
                                  robot'' = Robot di dj 2 'X'
                                  nbaby = Baby di dj 2
                                  carriedBaby = findBabyAt (di, dj) babies
                                  babies' = delete carriedBaby babies
                                  babies'' = nbaby:babies'
                                  in
                                    return (board', robot'', babies'')
                              else
                                return (board', robot', babies)
                  else let --dfs ignoring the babies, since the jail is no longer accesible
                    (_:path, value) = dfsOptimalPathNoBaby (board, (i,j), [], ([],0), ([],-10000))
                    in 
                      do
                        print path
                        let (board', robot') = followPath board robot path 2
                          in return (board', robot', babies)
          
          --looking for babyJail
          2 ->
            if jail_reachable
              then let
                objPath = lookForBabyJail board robot
                in
                  if null objPath
                    then
                      return (board, robot, babies)
                    else
                      let
                        src:path = objPath
                        l = length path
                        in
                          if l == 1
                            then
                              let
                                dest = last path
                                (board', robot', babies') = depositBaby board robot babies dest
                                in return (board', robot', babies') --mejorable
                            else
                              let
                                (srci, srcj) = src
                                (desti, destj) = head path
                                nbaby = Baby desti destj 2
                                carriedBaby = findBabyAt (srci, srcj) babies
                                babies' = delete carriedBaby babies
                                babies'' = nbaby:babies'
                                (board', robot') = followPath board robot path 2
                              in return (board', robot', babies'')
              else  -- acá tocaría soltar rápido al bebé de ser posible, pero kepereza
                return (board, robot, babies)
          _ -> return (board, robot, babies)



-- dfsOptimalPath :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> ([(Int, Int)],Int) -> ([(Int, Int)], Int) -> ([(Int, Int)], Int)
dfsOptimalPath :: ([[Char]], (Int, Int), [(Int, Int)], ([(Int, Int)],Int), ([(Int, Int)], Int)) -> ([(Int, Int)], Int)
dfsOptimalPath (board, node, visited, (current_path, current_value), (best_path, best_value)) =
  let
    l = length current_path
    (i,j) = node
    item = board!!i!!j
    in
      if l >= 10 || item == 'O' || item == 'Z' || node `elem` visited
        then (best_path, best_value)
        else
          let
            value = if item == 'X' then -1 else if item == 'C' then 50 else if item == 'B' then 101 else -1000 -- -1000 stands for...idk, but just in case
            visited' = (i,j):visited
            current_path' = current_path++[(i,j)]
            current_value' = current_value + value
            adjs = getAdjacents board (i,j) visited' ['O', 'Z']
            (best_path', best_value') = if current_value' > best_value then (current_path', current_value') else (best_path, best_value)
            in case adjs of
              [] -> (best_path', best_value')
              _  -> let
                fixed_adjs = dfsRecTuples adjs board visited' (current_path', current_value') (best_path', best_value')
                adj_best_paths = map dfsOptimalPath fixed_adjs
                in bestDfsPath adj_best_paths

dfsOptimalPathNoBaby :: ([[Char]], (Int, Int), [(Int, Int)], ([(Int, Int)],Int), ([(Int, Int)], Int)) -> ([(Int, Int)], Int)
dfsOptimalPathNoBaby (board, node, visited, (current_path, current_value), (best_path, best_value)) =
  let
    l = length current_path
    (i,j) = node
    item = board!!i!!j
    in
      if l > 10 || item == 'O' || item == 'Z' || item == 'B' || node `elem` visited
        then (best_path, best_value)
        else
          let
            value = if item == 'X' then -1 else if item == 'C' then 50 else -1000 -- -1000 stands for...idk, but just in case
            visited' = (i,j):visited
            current_path' = current_path++[(i,j)]
            current_value' = current_value + value
            adjs = getAdjacents board (i,j) visited' ['O', 'Z', 'B']
            (best_path', best_value') = if current_value' > best_value then (current_path', current_value') else (best_path, best_value)
            in case adjs of
              [] -> (best_path', best_value')
              _  -> let
                fixed_adjs = dfsRecTuples adjs board visited' (current_path', current_value') (best_path', best_value')
                adj_best_paths = map dfsOptimalPath fixed_adjs
                in bestDfsPath adj_best_paths


-- receives a list of tuples (path, path_value) and return the path with the highest value
bestDfsPath :: [([(Int, Int)], Int)] -> ([(Int, Int)], Int)
bestDfsPath [l] = l
bestDfsPath paths =
  let
    max:rec_paths = paths  -- assume max is the current, look for the max recursively and then compare the max of the tail with the current, assumed as the max
    (max_p, max_v) = max
    (max_rec_path, max_rec_value) = bestDfsPath rec_paths
    in
      if max_v >= max_rec_value
        then
          (max_p, max_v)
        else
          (max_rec_path, max_rec_value)


dfsRecTuples :: [(Int, Int)] -> [[Char]] -> [(Int, Int)] -> ([(Int, Int)], Int) -> ([(Int, Int)], Int) -> [([[Char]], (Int, Int), [(Int, Int)], ([(Int, Int)], Int), ([(Int, Int)], Int))]
dfsRecTuples [] _ _ _ _ = []
dfsRecTuples (node:t) board visited currentPath bestPath =
  let
    rec_call = dfsRecTuples t board visited currentPath bestPath
    in
      (board, node, visited, currentPath, bestPath):rec_call

pprintQueue :: [[(Int, Int)]] -> IO ()
pprintQueue [] = return ()
pprintQueue (path:queueTail) =
  do
    print path
    pprintQueue queueTail