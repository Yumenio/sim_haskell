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
                                        | otherwise = let m = length board; n = length $ head board; x = runRandom rand seed; y = runRandom rand x; xMod = rem x m; yMod = rem y n; nboard = subNth0 board xMod yMod 'O' in generateObstaclesAux nboard (amount-1) y 

genBabyJail :: [[Char]] -> Int -> Int -> [[Char]]
genBabyJail board babyCount doneCount = if babyCount == doneCount then board else let (i,j) = getBoardIndex board doneCount; nboard = subNth0 board i j 'S' in genBabyJail nboard babyCount (doneCount+1)

generateRobots :: [[Char]] -> Int -> Int -> [[Char]]
generateRobots board amount seed = let m = length board; n = length $ head board; x = runRandom rand seed; xMod = rem x; y = runRandom rand x; xMod = rem y n in if board!!xMod!!yMod !! 'X' then generateRobots board amount y else let board' = subNth0 board x y 'R' in generateRobots board (amount-1) y

simulate :: Int -> Int -> Int -> IO()
simulate m n seed = let x = 'X'; board = initBoard m n x; (board', seed') = generateObstacles board seed; board'' = genBabyJail board' 3 0 in pprint board''


main :: IO ()
main = simulate 5 5 42