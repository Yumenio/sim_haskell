import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random)
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

