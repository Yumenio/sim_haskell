module Random where
import Control.Monad.State (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, randomR, Random, randomIO)
import Control.Applicative ((<$>))

-- runRandom rand <seed>

type R a = State StdGen a

runRandom :: R a -> Int -> a
runRandom action seed = evalState action $ mkStdGen seed

rand :: R Int
rand = do
  gen <- get
  let (r, gen') = randomR (0,99) gen 
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