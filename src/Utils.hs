module Utils where
import Random (runRandom, rand)
import Data.List
import Data.Set

fill x times  | times == 1 = [x]
              | times < 1 = []
              | otherwise = x : fill x (times-1)

initBoard m n x  | m == 1 = [fill x n]
                  | m < 1 =  []
                  | otherwise = fill x n : initBoard (m-1) n x

subNth0 board i j x = 
  let
    (h, row:rs) = Data.List.splitAt i board;
    (rhead, _:rtail) = Data.List.splitAt j row;
    newrow = rhead++[x]++rtail
    in h++[newrow]++rs

getBoardIndex :: [[Char]] -> Int -> (Int, Int)
getBoardIndex board straightLineIndex =
  let
    n = length $ head board;
    i = div straightLineIndex n;
    j = rem straightLineIndex n;
    in (i,j)

randomAdj :: [[Char]] -> Int -> Int -> Int -> (Int, Int, Int)
randomAdj board i j seed =
  let
    r = runRandom rand seed
    rMod = mod r 4
    in
      case rMod of
        0 -> if validPos board i (j+1) then (i, j+1, r) else randomAdj board i j r
        1 -> if validPos board (i+1) j then (i+1, j, r) else randomAdj board i j r
        2 -> if validPos board i (j-1) then (i, j-1, r) else randomAdj board i j r
        3 -> if validPos board (i-1) j then (i-1, j, r) else randomAdj board i j r
        _ -> (i, j, r) -- should not happen but who knows x)

randomAdj8 :: [[Char]] -> Int -> Int -> Int -> (Int, Int, Int)
randomAdj8 board i j seed =
  let
    r = runRandom rand seed
    rMod = mod r 8
    in
      case rMod of
        0 -> if validPos board i (j+1) then (i, j+1, r) else randomAdj board i j r --right
        1 -> if validPos board (i+1) j then (i+1, j, r) else randomAdj board i j r --down
        2 -> if validPos board i (j-1) then (i, j-1, r) else randomAdj board i j r --left
        3 -> if validPos board (i-1) j then (i-1, j, r) else randomAdj board i j r --up

        4 -> if validPos board (i+1) (j+1) then (i+1, j+1, r) else randomAdj board i j r --down+right
        5 -> if validPos board (i+1) (j-1) then (i+1, j-1, r) else randomAdj board i j r --down+left
        6 -> if validPos board (i-1) (j-1) then (i-1, j-1, r) else randomAdj board i j r -- up+left
        7 -> if validPos board (i-1) (j+1) then (i-1, j+1, r) else randomAdj board i j r -- up+right
        _ -> (i, j, r) -- should not happen but who knows x)


validPos :: [[Char]] -> Int -> Int -> Bool
validPos board i j =
  let
    m = length board
    n = length $ head board
    in
      i >= 0 && i < m && j >= 0 && j < n

validPosMap :: ([[Char]], Int, Int, [Char]) -> Bool
validPosMap (board, i, j, excl) =
  validPos board i j && notElem (board!!i!!j) excl

rowDim :: (Foldable f, Num b) => f a -> b
rowDim = Data.List.foldr (\ x -> (+) 1) 0

pprint :: [[Char]] -> IO ()
pprint [] = putStrLn ""
pprint (r:t) = do
  print r
  pprint t


dirt :: [Char] -> Int
dirt [] = 0
dirt ('C':t) = 1 + dirt t
dirt (_:t) = 0 + dirt t

dirtPerc :: [[Char]] -> Int
dirtPerc board = let m = length board; n = length $ head board in div (sum (Data.List.map dirt board)*100) (m*n)


cleanPerc board = 100 - dirtPerc board


adjacents :: (Int, Int) -> (Int, Int) -> Bool
adjacents (i,j) (di, dj)  | i == di = j == (dj-1) || j == (dj+1)
                          | j == dj = i == (di-1) || i == (di+1)
                          | otherwise = False


getAdjacents :: [[Char]] -> (Int, Int) -> [(Int, Int)] -> [Char] -> [(Int, Int)]
getAdjacents board (i,j) visited excl =
  let
    candidates = [(board,i,j+1,excl), (board,i,j-1,excl), (board,i+1,j,excl), (board,i-1,j,excl)]
    validCandidates =  Data.List.filter validPosMap candidates
    validCandidates' = Data.List.map (\(_,i,j,_) -> (i,j)) validCandidates
    in
      validCandidates' Data.List.\\ visited


-- initBoard m n x  | m == 1 = [fill x n]
--                   | m < 1 =  []
--                   | otherwise = fill x n : initBoard (m-1) n x

dijkstra :: [[Char]] -> (Int, Int) -> [(Int, Int)]
dijkstra board (i,j) =
  let
    m = length board
    n = length $ head board
    bcosts = initBoard m n 100000
    costs = subNth0 bcosts i j 0
    vis = empty
    minCostPaths = iterateDijk board costs vis (m*n)
    in
      [(1,1)]


iterateDijk :: [[Char]] -> [[Int]] -> Set (Int, Int) -> Int -> [[Int]]
iterateDijk board costs vis totalNodes =
  if length vis == totalNodes
    then
      costs
    else let
      x = searchNextDijk costs vis
      vis' = Data.Set.insert x vis
      adjx = getAdjacents board x (toList vis) ['O', 'Z', 'R']
      costs' = updateCosts board costs x adjx
      in
        iterateDijk board costs' vis' totalNodes


searchNextDijk :: [[Int]] -> Set (Int, Int) -> (Int, Int)
searchNextDijk board vis =
  let
    m = length board
    n = length $ head board
    in
      searchNextDijkAux (0,0) (100001,(-1,-1)) board vis

searchNextDijkAux :: (Int, Int) -> (Int, (Int, Int)) -> [[Int]] -> Set (Int, Int) -> (Int, Int)
searchNextDijkAux (ci,cj) (best,(bi,bj)) board vis =
  let
    m = length board
    n = length $ head board
    in
      if ci == m
        then (bi,bj)
        else
          if cj == n
            then searchNextDijkAux (ci+1, 0) (best,(bi,bj)) board vis
            else
              if board!!ci!!cj < best && not (member (ci,cj) vis)
                then
                  searchNextDijkAux (ci, cj+1) (board!!ci!!cj, (ci,cj)) board vis
                else
                  searchNextDijkAux (ci, cj+1) (best, (bi, bj)) board vis

updateCosts :: [[Char]] -> [[Int]] -> (Int, Int) -> [(Int, Int)] -> [[Int]]
updateCosts _ costs _ [] = costs
updateCosts board costs (i,j) (adj:adjs) =
  let
    nodeCost = costs!!i!!j
    -- newCosts = Data.List.map (\(i,j) -> ((i,j),nodeCost + costs!!i!!j) ) adjs
    (adjx, adjy) = adj
    elem = board!!adjx!!adjy
    in
      do
      case elem of
        'X' -> let
          adjCost = 1000
          newCost = adjCost + nodeCost
          in
            if newCost < costs!!adjx!!adjy
              then let
                costs' = subNth0 costs adjx adjy newCost
                in updateCosts board costs' (i,j) adjs
              else
                updateCosts board costs (i,j) adjs
        'C' -> let
          adjCost = 100
          newCost = adjCost + nodeCost
          in
            if newCost < costs!!adjx!!adjy
              then let
                costs' = subNth0 costs adjx adjy newCost
                in updateCosts board costs' (i,j) adjs
              else
                updateCosts board costs (i,j) adjs
        'B' -> let
          adjCost = 10
          newCost = adjCost + nodeCost
          in
            if newCost < costs!!adjx!!adjy
              then let
                costs' = subNth0 costs adjx adjy newCost
                in updateCosts board costs' (i,j) adjs
              else
                updateCosts board costs (i,j) adjs
        _ -> error "found an unintended adjacent when updating the costs of dijkstra"

        
    
    -- in
    --   Data.List.map (\((i,j),ncost) -> if costs!!i!!j > ncost then subNth0 costs i j ncost else)