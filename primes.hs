-- An example of programming with streams.

import WF.Types
import WF.Primitives
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
-- Lifted stream functions
--

sDivides :: (Streamer s) => s [Int] -> s Int -> s Bool
sDivides = lifter2 dividesByN

sAccept :: (Streamer s) => s Int -> s Bool -> s (Maybe Int)
sAccept = lifter2 accept

sNot :: (Streamer s) => s Bool -> s Bool
sNot = lifter not


--
-- Other Stream primitives
--

memorize :: [a] -> Maybe a -> [a]
memorize mems (Just x)  = x:mems
memorize mems (Nothing) = mems

sMemory :: (Streamer s) => s [Int] -> s (Maybe Int) -> s [Int]
sMemory = lifter2 memorize

sDebubble :: (Streamer s) => s (Maybe a) -> s a
sDebubble s = newStream (db s)
  where db st = let (sf,sr) = sAdvance st
                in if (isNothing sf) then (db sr)
	        else (fromJust sf) : (db sr)

--
-- Main
--

primes :: (Streamer s) => s Int
primes = let ns = newStream [2..] 
             empty = newStream [[]] 
             (ns1,ns2) = dup ns
             mem = sMemory (selectLtR empty mem1) p1 
             (mem1,mem2) = dup mem
             divs = sDivides mem2 ns1 
             ndivs = sNot divs
             accepted = sAccept ns2 ndivs 
             primes = seededStream [Nothing] accepted 
             (p1,p2) = dup primes
         in sDebubble p2

main :: IO ()
main = do
  (f,r) <- return $ sAdvanceN 100 (primes :: EStream Int)
  print $ f
