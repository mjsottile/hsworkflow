import qualified WF.Primitives as Prim
import WF.Types
import Data.IORef
import System.Random
import System.IO.Unsafe

-- initialize a random number generator based on a seed
initializer :: Int -> IO (IORef StdGen)
initializer seed = do
  let gen = mkStdGen seed
  ior <- newIORef gen
  return ior

-- Function to lift the primitive addition operator
-- to a stream model.
adder :: (Num a, Streamer s) => s a -> s a -> s a
adder = Prim.lifter2 (+)

-- sampler that takes a range and a IORef containing a StdGen,
-- and returns a value after updating the StdGen in the IORef
sampler :: (Num a, Random a) => (a, a) -> IORef StdGen -> IO a
sampler (lo,hi) ior = do
  gen <- readIORef ior
  let (val,gen') = randomR (lo,hi) gen
--  putStrLn $ "\"" ++ (show val) ++ "\""
  writeIORef ior gen'
  return val

-- sampler activity.  
samplerActivity :: (Random a, Num a, Streamer s) => 
                   (a,a) -> (s (IORef StdGen) -> s a)
samplerActivity range = Prim.lifterIO rangedsampler
  where rangedsampler = sampler range

-- Define a workflow that adds pairs of random numbers.  we get
-- different behavior if we use effectful vs pure streams.  if we
-- use pure streams, then the effect is to sample the random number
-- generator once and add the value to itself.  if we use effectful
-- streams, the random number stream is sampled twice - once for
-- each argument of the adder.  uncomment the putStrLn in the
-- sampler function to see the difference in how the random number
-- generator is sampled.
wflow :: (Random a, Num a, Streamer s) => IORef StdGen -> (a,a) -> s a
wflow ior range = 
  let generator = (samplerActivity range) iorStream
      add = adder s1 s2
      iorStream = Prim.constantStream ior
      (s1,s2) = Prim.split generator
  in add
  
main :: IO ()
main = do
  ior <- initializer 1
  (f,r) <- return $ sAdvanceN 10 ( (wflow ior (1,6) ):: EStream Int)
  print $ f
  (f,r) <- return $ sAdvanceN 10 ( (wflow ior (1,6) ):: PStream Int)
  print $ f

