-- Fibonacci numbers


{-# OPTIONS_GHC -fglasgow-exts #-}
module FIB where
import WF.Primitives 
import WF.Types 
import Maybe 


adder = lifter2 (+)
 
fib = 
  let 
    wire7 = [0] 
    wire8 = [1] 
    wire0 = toEStream $ seededStream wire7 wire2 
    wire1 = toEStream $ seededStream wire8 wire6 
    wire4 = toEStream $ adder wire0 wire3 
    (wire2,wire3) = dup wire1 
    (wire5,wire6) = dup wire4 
  in wire5 

main :: IO ()
main = do
  print $ sTake 10 (toEStream fib)

main2 :: IO ()
main2 = do
  let f = (toEStream fib)
  print $ sTake 10 f
  print $ sTake 10 f