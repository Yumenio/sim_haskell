module Random where
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, random, randomR, Random, randomIO)
import Control.Applicative ((<$>))

-- runRandom rand <seed>

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
  gen <- get
  let (r, gen') = random gen
  put gen'
  return r


-- shouldn't use with a little interval for a long period of time, lest the result and the seed are equals and no subsequent random number is generated
-- a decent workaround, use the rand function and then calculate the modulo, but use as seed the original number, not the result of the mod operation
randR :: Int -> Int -> R Int
randR a b = do
  gen <- get
  let (r, gen') = randomR (a,b) gen 
  put gen'
  return r


-- type Rand a = State StdGen a  

-- getRandom :: (Random a) => Rand a
-- getRandom = get >>= (\r -> let (a,g) = random r in (put g) >> (return a))

-- runRand :: Int -> Rand a -> a
-- runRand n r = evalState r $ mkStdGen n

-- runRandIO :: Rand a -> IO a
-- runRandIO r = randomIO >>= (\rnd -> return $ runRand rnd r)

-- getRandoms :: (Random a) => Int -> Rand [a]
-- getRandoms n = mapM (\_ -> getRandom) [1..n]