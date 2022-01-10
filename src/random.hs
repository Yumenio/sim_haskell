import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random, randomRIO)
import Control.Applicative ((<$>))

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Double
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r

randIO_filter :: Int -> Int
randIO_filter r = r

randIO :: IO Int
randIO = do
  r <- randomRIO (1,100)
  let r2 = randIO_filter r
  return r2


-- aux_func :: IO Int
aux_func = randIO