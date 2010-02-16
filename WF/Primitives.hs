{-
   module of workflow primitives based on streams

   matt\@cs.uoregon.edu
   ghulette\@cs.uoregon.edu
-}

module WF.Primitives where

import WF.Types
import System.IO.Unsafe
import Maybe

--
-- =======================================================================
--  Start of lifters
--

{-|
   Lifters take pure functions and turn them into streamified ones.
-}

-- | Apply a pure function to a stream to yield a list, then wrap
--   that in a stream.
lifter :: (Streamer s1, Streamer s2) 
       => (a -> b) -> s1 a -> s2 b
lifter f s = newStream (appf s)
  where appf s' = let (fr,r) = sAdvance s' in (f fr):(appf r)

-- | lifter for two arguments.  see appf2
lifter2 :: (Streamer s1, Streamer s2, Streamer s3)  
        => (a -> b -> c) -> s1 a -> s2 b -> s3 c
lifter2 f s1 s2 = newStream (appf2 s1 s2)
  where appf2 s1' s2' = 
          let (fr1, r1) = sAdvance s1'
              (fr2, r2) = sAdvance s2' in
            (f fr1 fr2):(appf2 r1 r2)
-- 
-- -- | lifter for three arguments
-- lifter3 :: (Streamer a b, Streamer a' b', Streamer a'' b'', Streamer c d) 
--         => (b -> b' -> b'' -> d) 
--         -> a -> a' -> a''
--         -> c
-- lifter3 f s1 s2 s3 = newStream (appf3 s1 s2 s3)
--   where appf3 s1' s2' s3' = 
--           let (fr1, r1) = sAdvance s1'
--               (fr2, r2) = sAdvance s2'
--               (fr3, r3) = sAdvance s3' in
--             (f fr1 fr2 fr3):(appf3 r1 r2 r3)
-- 
-- -- | lifter for four arguments
-- lifter4 :: (Streamer a b, Streamer a' b', Streamer a'' b'', 
--             Streamer a''' b''', Streamer c d)
--         => (b -> b' -> b'' -> b''' -> d)
--         -> a -> a' -> a'' -> a'''
--         -> c
-- lifter4 f s1 s2 s3 s4 = newStream (appf4 s1 s2 s3 s4)
--   where appf4 s1' s2' s3' s4' =
--        let (fr1, r1) = sAdvance s1'
--            (fr2, r2) = sAdvance s2'
--            (fr3, r3) = sAdvance s3'
--            (fr4, r4) = sAdvance s4' in
--          (f fr1 fr2 fr3 fr4):(appf4 r1 r2 r3 r4)
-- 
-- -- | lifter for five arguments
-- lifter5 :: (Streamer a b, Streamer a' b', Streamer a'' b'', 
--             Streamer a''' b''', Streamer a'''' b'''', Streamer c d)
--         => (b -> b' -> b'' -> b''' -> b'''' -> d)
--         -> a -> a' -> a'' -> a''' -> a''''
--         -> c
-- lifter5 f s1 s2 s3 s4 s5 = newStream (appf5 s1 s2 s3 s4 s5)
--   where appf5 s1' s2' s3' s4' s5' =
--        let (fr1, r1) = sAdvance s1'
--            (fr2, r2) = sAdvance s2'
--            (fr3, r3) = sAdvance s3'
--            (fr4, r4) = sAdvance s4' 
--                   (fr5, r5) = sAdvance s5' in
--          (f fr1 fr2 fr3 fr4 fr5):(appf5 r1 r2 r3 r4 r5)

--
--  End of lifters
-- =======================================================================
--

--
-- =======================================================================
--  Start of combinators
--


{-|
   Activity that takes two streams as input and provides elements from
   the left (i.e., the first stream) unless it is empty, at which time
   it provides elements from the right until the left becomes un-empty
   again.
-}
selectLtR :: Streamer s => s a -> s a -> s a
selectLtR xs ys = newStream (choose xs ys)
  where choose l r = if sEmpty l
                     then let (rf,rr) = sAdvance r in (rf : choose l rr)
                     else let (lf,lr) = sAdvance l in (lf : choose lr r)

{-|
   Duplicate a stream and create two new, independent streams with identical
   contents as the original.
-}
dup :: Streamer s => s a -> (s a,s a)
dup s = 
  let (l1,l2) = unzip (dupList s) 
  in (newStream l1, newStream l2)
  where 
    dupList st = let (hd,tl) = sAdvance st 
                 in (hd,hd) : (dupList tl)

{-|
   Split a stream into two.  If spitting an effectful stream, the effects
   will be seen by both recipients.  Use dup to avoid this if desired.
-}
split :: Streamer s => s a -> (s a,s a)
split s = (left,right)
  where left  = lifter id s
        right = lifter id s

{-|
   Alternator split
-}
altsplit :: (Streamer s) => s a -> (s a,s a)
altsplit s = 
  let (l1,l2) = unzip(altList s)
  in (newStream l1, newStream l2)
  where
    altList st = let (h1,t1) = sAdvance st in
                 let (h2,t2) = sAdvance t1
                 in (h1,h2):(altList t2)

{-|
   Alternator merge
-}
altmerge :: (Streamer s) => s a -> s a -> s a
altmerge xs ys = newStream (alt xs ys)
  where alt l r = let (lf,lr) = sAdvance l in lf : (alt r lr)

{-|
   zipper merge
-}
zipper :: (Streamer s1, Streamer s2, Streamer s3)
       => s1 a -> s2 a -> s3 (a,a)
zipper xs ys = newStream (zipped xs ys)
  where zipped l r = let (lf,lr) = sAdvance l in
                     let (rf,rr) = sAdvance r
                     in (lf,rf) : (zipped lr rr)

{-|
   A stream that forever provides a single constant.
-}
constantStream :: (Streamer s) => a -> s a
constantStream c = newStream (repeat c)

{-|
   Provide an initial list for the stream to start with and produce an
   activity that will consume a stream and emit a new stream with the
   initial list at the front of the stream passed in.  Note that if the
   seed list is infinite, the stream being passed in will never be 
   serviced, so beware.
-}
seededStream :: (Streamer s) => [a] -> s a -> s a
seededStream l s = selectLtR (newStream l) s

{-|
   Produce a monotonically increasing counter with a starting value.
-}
counterStream :: (Enum a, Streamer s) => a -> s a
counterStream start = newStream ([start..])
