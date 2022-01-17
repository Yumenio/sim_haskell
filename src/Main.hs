module Main where
import System.Random
import Distribution.Simple.Program.HcPkg (list)
import Random
import Utils (initBoard, getBoardIndex, pprint, subNth0)
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
    board' = subNth0 board xMod yMod 'C'
    in
      generateInitialDirtAux board' (amount-1) y


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


simulate :: Int -> Int -> Int -> IO()
simulate m n seed =
  do
    let
      t = 2
      x = 'X'
      perc = 10
      board = initBoard m n x
      (board', babies, seed') = generateEnvironment board seed perc
      (fullBoard, robots, seed'') = generateRobots board' 1 seed'
    print "Initial board:"
    pprint fullBoard
      
    let
      (envBoard, babies', seed''') = simulateEnvironment 1 fullBoard babies seed''
    print "Board after environment sim:"
    pprint envBoard

    let
      robot = head robots
      (reacBoard, robot', babies'') = reactiveAgent envBoard robot babies'

    print "Board after reactive agent"
    pprint reacBoard

generateEnvironment board seed perc =
  do
    let
      x = 'X'
      perc = 10
      -- t = 2
      -- board = initBoard m n x
      (dirtiedBoard, seed') = generateInitialDirt board 7 seed
      (board', seed'') = generateObstacles dirtiedBoard perc seed'
      board'' = genBabyJail board' 3 0
      (board''', babies, seed''') = generateBabies board'' 3 seed''
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
main = simulate 5 5 42