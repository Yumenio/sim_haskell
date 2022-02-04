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

generateSporadicDirt :: [[Char]] -> Int -> Int -> Int -> Int -> ([[Char]], Int)
generateSporadicDirt board current_cycle t perc seed =
  let
    reg = mod current_cycle t
    in
      if reg == 0
        then let
          m = length board
          n = length $ head board
          -- generating 1+perc just in case perc==0
          amount = succ ( div (m*n*perc) 100)
          in
            generateInitialDirtAux board amount seed
        else
          (board,seed)

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


simulate :: [Char] -> Int -> Int -> Int -> Int -> Int -> IO()
simulate robotType m n t cycles seed =
  do
    let
      x = 'X'
      perc = 80
      board = initBoard m n x
      (board', babies, seed') = generateEnvironment board seed perc
      (fullBoard, robots, seed'') = generateRobots board' 2 seed'
    print "Initial board:"
    pprint fullBoard

    simulationLoop robotType 1 cycles t fullBoard robots babies seed''
    



simulationLoop :: [Char] -> Int -> Int -> Int -> [[Char]] -> [Robot] -> [Baby] -> Int -> IO ()
simulationLoop rType cycle tcycle t board robots babies seed =
    if cycle == tcycle
      then return ()
      else do
        print "CYCLE"
        print cycle
        let
          (boardd,seedd) = generateSporadicDirt board cycle t 7 seed
          cleanPercentage = cleanPerc boardd
          in
            do
              putStr "Clean % = "
              print cleanPercentage
              let
                (envBoard, babies', seed') = simulateEnvironment 1 boardd babies seedd
              print "Board after environment sim:"
              -- print babies'
              pprint envBoard
              if cleanPercentage < 80
                then let
                  (reacBoard, robots', babies'') = simulateRobotsTimes t rType envBoard robots babies'
                  in do
                    print "Board after agents"
                    pprint reacBoard
                    simulationLoop rType (cycle+1) tcycle t reacBoard robots' babies'' seed'
                else let
                  (reacBoard, robots', babies'') = (envBoard, robots, babies')
                  in do
                    print "Board after agents"
                    pprint reacBoard
                    simulationLoop rType (cycle+1) tcycle t reacBoard robots' babies'' seed'
              -- print "Board after agents"
              -- -- print robots
              -- -- print babies''
              -- pprint reacBoard
              -- simulationLoop rType (cycle+1) tcycle t reacBoard robots' babies'' seed'


generateEnvironment board seed perc =
  do
    let
      x = 'X'
      m = length board
      n = length $ head board
      babyCount = 1 + div (m*n*10) 100
      -- babyCount = 1
      -- board = initBoard m n x
      (dirtiedBoard, seed') = generateInitialDirt board perc seed
      (board', seed'') = generateObstacles dirtiedBoard 7 seed'
      board'' = genBabyJail board' babyCount 0
      (board''', babies, seed''') = generateBabies board'' babyCount seed''
      in
        (board''', babies, seed''')


simulateEnvironment :: Int -> [[Char]] -> [Baby] -> Int -> ([[Char]], [Baby], Int)
simulateEnvironment 0 board babies seed = (board, babies, seed)
simulateEnvironment cycles board babies seed =
  let
    (dirtiedBoard, seed') = generateSimpleDirt board babies seed
    (board', babies', seed'') = moveBabies dirtiedBoard babies seed'
    in
      simulateEnvironment (cycles-1) board' babies' seed''


simulateRobotsM :: [[Char]] -> [Robot] -> [Baby] -> [Robot] -> ([[Char]], [Robot], [Baby])
simulateRobotsM board [] babies simulatedRobots = (board, simulatedRobots, babies)
simulateRobotsM board (robot:rs) babies simRobots =
  let
    (board', robot', babies') = modelBasedAgent board robot babies
    in
      simulateRobotsM board' rs babies' (simRobots++[robot'])



simulateRobotsDijk :: [[Char]] -> [Robot] -> [Baby] -> [Robot] -> ([[Char]], [Robot], [Baby])
simulateRobotsDijk board [] babies simulatedRobots = (board, simulatedRobots, babies)
simulateRobotsDijk board (robot:rs) babies simRobots =
  let
    (board', robot', babies') = dijkstraBasedAgent board robot babies
    in
      simulateRobotsDijk board' rs babies' (simRobots++[robot'])


simulateRobotsTimes :: Int -> [Char] -> [[Char]] -> [Robot] -> [Baby] -> ([[Char]], [Robot], [Baby])
-- simulateRobotsTimes 0 rType board robots babies = (board, robots, babies)
simulateRobotsTimes cycles rType board robots babies
  | cycles <= 0 = (board, robots, babies)
  | otherwise =
    let
      (board', robots', babies') = simulateRobots rType board robots babies []
      cycles' = cycles - 1
      in
        simulateRobotsTimes cycles' rType board' robots' babies'

simulateRobots :: [Char] -> [[Char]] -> [Robot] -> [Baby] -> [Robot] -> ([[Char]], [Robot], [Baby])
simulateRobots rType board [] babies simulatedRobots = (board, simulatedRobots, babies)
simulateRobots rType board (robot:rs) babies simRobots
  | rType == "r" =
  let
    (board', robot', babies') = reactiveAgent board robot babies
    in
      simulateRobots rType board' rs babies' (simRobots++[robot'])
  | rType == "m" =
  let
    (board', robot', babies') = modelBasedAgent board robot babies
    in
      simulateRobots rType board' rs babies' (simRobots++[robot'])
  | rType == "d" =
  let
    (board', robot', babies') = dijkstraBasedAgent board robot babies
    in
      simulateRobots rType board' rs babies' (simRobots++[robot'])
  | otherwise = error "invalid robot type"


simulateRobotsR :: [[Char]] -> [Robot] -> [Baby] -> [Robot] -> ([[Char]], [Robot], [Baby])
simulateRobotsR board [] babies simulatedRobots = (board, simulatedRobots, babies)
simulateRobotsR board (robot:rs) babies simRobots =
  let
    (board', robot', babies') = modelBasedAgent board robot babies
    in
      simulateRobotsR board' rs babies' (simRobots++[robot'])


simulateRobotsMIO :: [[Char]] -> [Robot] -> [Baby] -> [Robot] -> IO ([[Char]], [Robot], [Baby])
simulateRobotsMIO board [] babies simulatedRobots =
  do
    print "FINAL CALL"
    print (board, simulatedRobots, babies)
    return (board, simulatedRobots, babies)
simulateRobotsMIO board (robot:rs) babies simRobots =
  do
    print "simulation of"
    print robot
    print "with"
    print babies
    let
      (board', robot', babies') = modelBasedAgent board robot babies
    print "yielded"
    print board'
    print robot'
    print babies'
    return (simulateRobotsM board' rs babies' (simRobots++[robot']))


main :: IO ()
main = simulate "d" 5 5 7 100 42