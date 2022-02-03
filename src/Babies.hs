module Babies where
import Utils (subNth0, getBoardIndex, randomAdj, randomAdj8, validPos)
import Random (rand, runRandom)

data Baby = Baby {babyRow :: Int, babyCol :: Int, babyState :: Int} deriving (Show, Eq)

babyCoor :: Baby -> (Int, Int)
babyCoor baby = (babyRow baby, babyCol baby)

babyAll :: Baby -> (Int, Int, Int)
babyAll baby = (babyRow baby, babyCol baby, babyState baby)

moveLeftB :: [[Char ]] -> Baby -> ([[Char]], Baby)
moveLeftB board baby =
  let
    (i,j,s) = babyAll baby
    in
      if s /= 1
        then
          error "Baby is not free"
        else
          if j == 0
            then (board,baby)
            else
              let
                board' = moveAnyB board i j 0 (-1) 'X'
                baby' = Baby i (j-1) s
                in (board', baby')


moveUpB :: [[Char ]] -> Baby -> ([[Char]], Baby)
moveUpB board baby =
  let
    (i,j,s) = babyAll baby
    in
      if s /= 1
        then
          error "Baby is not free"
        else
          if i == 0
            then (board,baby)
            else
              let
                board' = moveAnyB board i j (-1) 0 'X'
                baby' = Baby (i-1) j s
                in (board', baby')


moveRightB :: [[Char]] -> Baby -> ([[Char]], Baby)
moveRightB board baby =
  let
    (i,j,s) = babyAll baby
    n = length $ head board
    in
      if s /= 1
        then
          error "Baby is not free"
        else
          if j == (n-1)
            then (board,baby)
            else
              let
                board' = moveAnyB board i j 0 1 'X'
                baby' = Baby i (j+1) s
                in (board', baby')


moveDownB :: [[Char]] -> Baby -> ([[Char]], Baby)
moveDownB board baby =
  let
    (i,j,s) = babyAll baby
    m = length board
    in
      if s /= 1
        then
          error "Baby is not free"
        else
          if i == m
            then (board,baby)
            else
              let
                board' = moveAnyB board i j 1 0 'X'
                baby' = Baby (i+1) j s
                in (board', baby')


-- cleanR board i j = 

moveAnyB board i j deltaI deltaJ sub =
  if canMoveB board (i,j) (i+deltaI,j+deltaJ)
    then
      let
        oldItem = board!!i!!j;
        board' = subNth0 board (i+deltaI) (j+deltaJ) oldItem
        in subNth0 board' i j sub
    else
      board


-- canMoveB :: [[Char]] -> Int -> Int -> Bool
-- canMoveB board i j =
--   let
--     elem = board!!i!!j
--     in  -- O => Obstacle, C => Crap,    R => Robot,     B => Baby,    S => BabyJail,  Z => Baby in jail xd
--       not (elem == 'O' || elem == 'C' || elem == 'R' || elem == 'B' || elem == 'S' || elem == 'Z')



canMoveB :: [[Char]] -> (Int,Int) -> (Int,Int) -> Bool
canMoveB board (si,sj) (di,dj) =
  let
    elem = board!!di!!dj
    in  -- O => Obstacle, C => Crap,    R => Robot,     B => Baby,    S => BabyJail,  Z => Baby in jail xd
      if elem == 'O'
        then
          canPushObstacle board (si,sj) (di,dj)
        else
          not (elem == 'C' || elem == 'R' || elem == 'B' || elem == 'S' || elem == 'Z')

canPushObstacle board (fromi, fromj) (toi, toj) =
  let
    dx = toi - fromi
    dy = toj - fromj
    fi = toi + dx
    fj = toj + dy
    in
      validPos board fi fj && (
       if
        board!!fi!!fj == 'O'
        then
          canPushObstacle board (toi, toj) (fi, fj)
        else
          board!!fi!!fj == 'X'
      )

pushObstacle board (fromi, fromj) (toi, toj) =
  let
    dx = toi - fromi
    dy = toj - fromj
    fi = toi + dx
    fj = toj + dy
    in
      if board!!fi!!fj == 'X'
        then
          subNth0 board fi fj 'O'
        else
          pushObstacle board (toi, toj) (fi,fj)


genBabyJail :: [[Char]] -> Int -> Int -> [[Char]]
genBabyJail board babyCount doneCount =
  if babyCount == doneCount
    then board
    else
      let
        (i,j) = getBoardIndex board doneCount;
        nboard = subNth0 board i j 'S'
        in genBabyJail nboard babyCount (doneCount+1)




generateBabies :: [[Char]] -> Int -> Int -> ([[Char]], [Baby], Int)
generateBabies board amount seed = generateBabiesAux board amount seed []

generateBabiesAux :: [[Char]] -> Int -> Int -> [Baby] -> ([[Char]], [Baby], Int)
generateBabiesAux board 0 seed babies = (board, babies, seed)
generateBabiesAux board amount seed babies =
  let
    m = length board;
    n = length $ head board;
    x = runRandom rand seed; xMod = mod x m;
    y = runRandom rand x; yMod = mod y n
    in
      if board!!xMod!!yMod /= 'X'
        then generateBabiesAux board amount y babies
        else
          let
            board' = subNth0 board xMod yMod 'B'
            baby = Baby xMod yMod 1
            in generateBabiesAux board' (amount-1) y (baby:babies)

moveBabies :: [[Char]] -> [Baby] -> Int -> ([[Char]], [Baby], Int)
moveBabies board [] seed = (board, [], seed)
moveBabies board (baby:bs) seed =
  let
    (_,_,s) = babyAll baby
    in
      if s == 1
        then
          let
            (board', baby', seed') = moveBaby board baby seed
            (board'', bs', seed'') = moveBabies board' bs seed'
            in
              (board'', baby':bs', seed'')
        else
          let
            (board', bs', seed') = moveBabies board bs seed
            in
              (board', baby:bs', seed')

moveBaby :: [[Char]] -> Baby -> Int -> ([[Char]], Baby, Int)
moveBaby board baby seed = moveBabyAux board baby seed 1

moveBabyAux :: [[Char]] -> Baby -> Int -> Int -> ([[Char]], Baby, Int)
moveBabyAux board baby seed 0 = (board, baby, seed)
moveBabyAux board baby seed try =
  let
    (i,j,s) = babyAll baby
    (i', j', seed') = randomAdj board i j seed
    in
      if canMoveB board (i,j) (i', j')
        then
          if board!!i'!!j' == 'O'
            then let
              board' = pushObstacle board (i,j) (i',j')
              board'' = subNth0 board' i' j' 'B'
              board''' = subNth0 board'' i j 'X' --clear the old position
              baby' = Baby i' j' 1
              in
                (board''', baby', seed')
            else let
              board' = subNth0 board i' j' 'B'
              board'' = subNth0 board' i j 'X' --clear the old position
              baby' = Baby i' j' 1
              in
                (board'', baby', seed')
        else
          moveBabyAux board baby seed' (try-1)

--not considering all 3x3 squares in the board, I think that's lame af
generateSimpleDirt :: [[Char]] -> [Baby] -> Int -> ([[Char]], Int)
generateSimpleDirt board [] seed = (board, seed)
generateSimpleDirt board (baby:bs) seed =
  let
    (i,j,s) = babyAll baby
    (i', j', seed') = randomAdj board i j seed
    in
      if s == 2 || board!!i'!!j' /= 'X'
        then
          generateSimpleDirt board bs seed'
        else
          let
            board' = subNth0 board i' j' 'C'
            in
              generateSimpleDirt board' bs seed'

--for using with map function
genSingleDirt :: ([[Char]], Baby, Int) -> (Int, Int)
genSingleDirt (board, baby, seed) =
  let
    (i,j) = babyCoor baby
    (i', j', seed') = randomAdj board i j seed
    in
      if board!!i'!!j' /= 'X'
        then (-1,-1)
        else (i',j')


generateIntrincateDirt :: [[Char]] -> [Baby] -> Int -> ([[Char]], Int)
generateIntrincateDirt board [] seed = (board, seed)
generateIntrincateDirt board (baby:bs) seed =
  let
    (i,j,s) = babyAll baby
    (i', j', seed') = randomAdj8 board i j seed
    in
      if s == 2 || board!!i'!!j' /= 'X'
        then
          generateIntrincateDirt board bs seed'
        else let
          board' = subNth0 board i' j' 'C'
          in
            generateIntrincateDirt board' bs seed'