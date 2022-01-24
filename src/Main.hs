module Main where
import System.Random
import Distribution.Simple.Program.HcPkg (list)
import Random
import Utils (initBoard, getBoardIndex, pprint, subNth0, cleanPerc)
import Robot
import Babies

--  LEGEND
--'X'=Empty Cell
--'R'=Robot
--'B'=Baby
--'O'=Obstacle
--'S'=BabyJail
--'C'=Crap
--'Z'=Baby in Jail :D

--first of the pipeline
generateInitialDirt :: [[Char]] -> Int -> Int -> ([[Char]], Int)
generateInitialDirt board perc seed =
  let
    m = length board
    n = length $ head board
    amount = div (m*n*perc) 100
    in
      generateInitialDirtAux board amount seed

generateInitialDirtAux :: [[Char]] -> Int -> Int -> ([[Char]], Int)
generateInitialDirtAux board 0 seed  = (board, seed)
generateInitialDirtAux board amount seed =
  let
    m = length board
    n = length $ head board
    x = runRandom rand seed
    y = runRandom rand x
    xMod = mod x m
    yMod = mod y n
    cell = board!!xMod!!yMod
    in if cell == 'X'
      then let
        board' = subNth0 board xMod yMod 'C'
        in
          generateInitialDirtAux board' (amount-1) y
      else
        generateInitialDirtAux board (amount-1) y


--second of the pipeline
generateObstacles :: [[Char]] -> Int -> Int -> ([[Char]], Int)
generateObstacles board perc seed =
  let
    m = length board
    n = length $ head board
    amount = div (m*n*perc) 100
    in generateObstaclesAux board amount seed

generateObstaclesAux :: [[Char]] -> Int -> Int -> ([[Char]], Int)
generateObstaclesAux board amount seed  | amount == 0 = (board, seed)
                                        | otherwise =
                                          let
                                            m = length board;
                                            n = length $ head board;
                                            x = runRandom rand seed;
                                            y = runRandom rand x;
                                            xMod = mod x m;
                                            yMod = mod y n;
                                            nboard = subNth0 board xMod yMod 'O'
                                            in generateObstaclesAux nboard (amount-1) y 


simulateR :: Int -> Int -> Int -> Int -> IO()
simulateR m n cycles seed =
  do
    let
      t = cycles
      x = 'X'
      perc = 10
      board = initBoard m n x
      (board', babies, seed') = generateEnvironment board seed perc
      (fullBoard, robots, seed'') = generateRobots board' 1 seed'
    print "Initial board:"
    pprint fullBoard

    simulationLoop t fullBoard robots babies seed''      
    

simulateM :: Int -> Int -> Int -> Int -> IO()
simulateM m n cycles seed =
  do
    let
      t = cycles
      x = 'X'
      perc = 10
      board = initBoard m n x
      (board', babies, seed') = generateEnvironment board seed perc
      (fullBoard, robots, seed'') = generateRobots board' 1 seed'
    print "Initial board:"
    pprint fullBoard

    simulationLoop t fullBoard robots babies seed''      
    

simulationLoop :: Int -> [[Char]] -> [Robot] -> [Baby] -> Int -> IO ()
simulationLoop 0 board robots babies seed = return ()
simulationLoop t board robots babies seed =
  do
    print "CYCLE"
    print t
    let
      cleanPercentage = cleanPerc board
      in
        do
          putStr "Clean % = "
          print cleanPercentage
          let
            (envBoard, babies', seed') = simulateEnvironment 1 board babies seed

          print "Board after environment sim:"
          -- print babies'
          pprint envBoard

          let
            robot = head robots
            -- (reacBoard, robot', babies'') = reactiveAgent envBoard robot babies'
            (reacBoard, robot', babies'') = modelBasedAgent envBoard robot babies'
          print "Board after agent"
          -- print babies''
          -- print robot'
          pprint reacBoard
          simulationLoop (t-1) reacBoard [robot'] babies'' seed'


generateEnvironment board seed perc =
  do
    let
      x = 'X'
      perc = 10
      m = length board
      n = length $ head board
      babyCount = 1 + div (m*n*10) 100
      -- babyCount = 1
      -- board = initBoard m n x
      (dirtiedBoard, seed') = generateInitialDirt board 7 seed
      (board', seed'') = generateObstacles dirtiedBoard perc seed'
      board'' = genBabyJail board' babyCount 0
      (board''', babies, seed''') = generateBabies board'' babyCount seed''
      in
        (board''', babies, seed''')
        -- simulateEnvironment t board''' babies seed''


simulateEnvironment :: Int -> [[Char]] -> [Baby] -> Int -> ([[Char]], [Baby], Int)
simulateEnvironment 0 board babies seed = (board, babies, seed)
simulateEnvironment cycles board babies seed =
  let
    (dirtiedBoard, seed') = generateSimpleDirt board babies seed
    (board', babies', seed'') = moveBabies dirtiedBoard babies seed'
    in
      simulateEnvironment (cycles-1) board' babies' seed''


main :: IO ()
main = simulateM 5 5 100 42