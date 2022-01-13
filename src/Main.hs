module Main where
import System.Random
import Distribution.Simple.Program.HcPkg (list)
import Random
import Utils (initBoard, getBoardIndex, pprint, subNth0)
import Robot



generateObstacles :: [[Char]] -> Int -> ([[Char]], Int)
generateObstacles board seed =
  let m = length board; n = length $ head board; perc = 10; amount = div (m*n*perc) 100 in generateObstaclesAux board amount seed

generateObstaclesAux :: [[Char]] -> Int -> Int -> ([[Char]], Int)
generateObstaclesAux board amount seed  | amount == 0 = (board, seed)
                                        | otherwise = let m = length board; n = length $ head board; x = runRandom rand seed; y = runRandom rand x; xMod = mod x m; yMod = mod y n; nboard = subNth0 board xMod yMod 'O' in generateObstaclesAux nboard (amount-1) y 

genBabyJail :: [[Char]] -> Int -> Int -> [[Char]]
genBabyJail board babyCount doneCount = if babyCount == doneCount then board else let (i,j) = getBoardIndex board doneCount; nboard = subNth0 board i j 'S' in genBabyJail nboard babyCount (doneCount+1)

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
            robot = Robot xMod yMod 0
            in generateRobotsAux board' (amount-1) y (robot:robots)


simulate :: Int -> Int -> Int -> IO()
simulate m n seed =
  do
    let 
      x = 'X'; 
      board = initBoard m n x;
      (board', seed') = generateObstacles board seed;
      board'' = genBabyJail board' 3 0;
      (board''', robots, seed'') = generateRobots board'' 2 seed';
      (board4, robots', _) = moveRobots board''' robots seed''
    pprint board'''
    pprint board4


main :: IO ()
main = simulate 5 5 42