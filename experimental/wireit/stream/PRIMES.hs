-- Calculate primes


{-# OPTIONS_GHC -fglasgow-exts #-}
module PRIMES where
import WF.Primitives 
import WF.Types 
import Maybe 

--
-- Domain functions
--
divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

dividesByN :: [Int] -> Int -> Bool
dividesByN ys x = or (map (divides x) ys)

accept :: a -> Bool -> Maybe a
accept k True  = Just k
accept _ False = Nothing

--
-- Other Stream primitives
--
memorize :: [a] -> Maybe a -> [a]
memorize mems (Just x)  = x:mems
memorize mems (Nothing) = mems

sDebubble :: (Streamer a (Maybe b), Streamer c b) => a -> c
sDebubble s = newStream (db s)
  where db st = let sf = sFront st
                in if (isNothing sf) then (db $ sRest st)
	        else (fromJust sf) : (db $ sRest st) 

sDivides = lifter2 dividesByN
sAccept = lifter2 accept
sNot = lifter not
sMemory = lifter2 memorize
 
primes = 
  let 
    wire0 = toPStream (newStream [[]]) 
    wire10 = [Nothing] 
    wire14 = toPStream $ counterStream 2 
    wire11 = toPStream $ seededStream wire10 wire9 
    wire8 = toPStream $ selectLtR wire0 wire4 
    wire6 = toPStream $ sDivides wire5 wire1 
    wire9 = toPStream $ sAccept wire2 wire7 
    wire7 = toPStream $ sNot wire6 
    wire3 = toPStream $ sMemory wire8 wire13 
    (wire1,wire2) = dup wire14 
    (wire4,wire5) = dup wire3 
    (wire13,wire12) = dup wire11 
  in sDebubble wire12 

main :: IO ()
main = do
  print $ sTake 100 (toPStream primes)