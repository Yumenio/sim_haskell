module Main where
import System.Random
import Distribution.Simple.Program.HcPkg (list)
import Random
import Utils (initBoard, getBoardIndex, pprint, subNth0)
import Robot
import Babies



generateObstacles :: [[Char]] -> Int -> ([[Char]], Int)
generateObstacles board seed =
  let
    m = length board;
    n = length $ head board;
    perc = 10;
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