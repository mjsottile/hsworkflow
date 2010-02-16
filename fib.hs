import WF.Primitives
import WF.Types

-- Function to lift the primitive addition operator
-- to a stream model.
adder :: (Num a, Streamer s) => s a -> s a -> s a
adder = lifter2 (+)

-- Define a workflow that produces Fibonacci numbers
fib :: (Num a, Streamer s) => s a
fib = let lr1 = seededStream [0] db1
          lr2 = seededStream [1] da2
          (db1,db2) = dup lr2
          add = adder lr1 db2
          (da1,da2) = dup add
      in da1
  
main :: IO ()
main = do
  (f,r) <- return $ sAdvanceN 10 (fib :: PStream Int)
  print $ f
