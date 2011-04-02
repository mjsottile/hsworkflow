--
-- module of workflow primitives based on streams
--
-- matt@cs
--
{-# LANGUAGE FlexibleContexts #-}

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
lifter :: (Streamer a b,Streamer c d) 
       => (b -> d) 
       -> a 
       -> c
lifter f s = newStream (appf s)
  where appf s' = (f (sFront s')):(appf (sRest s'))

-- | lifter for two arguments.  see appf2
lifter2 :: (Streamer a b, Streamer a' b', Streamer c d)  
        => (b -> b' -> d) 
        -> a -> a'
        -> c
lifter2 f s1 s2 = newStream (appf2 s1 s2)
  where appf2 s1' s2' = (f (sFront s1') (sFront s2')):
                        (appf2 (sRest s1') (sRest s2'))

-- | lifter for three arguments
lifter3 :: (Streamer a b, Streamer a' b', Streamer a'' b'', Streamer c d) 
        => (b -> b' -> b'' -> d) 
        -> a -> a' -> a''
        -> c
lifter3 f s1 s2 s3 = newStream (appf3 s1 s2 s3)
  where appf3 s1' s2' s3' = (f (sFront s1') (sFront s2') (sFront s3')):
                            (appf3 (sRest s1') (sRest s2') (sRest s3'))

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
selectLtR :: Streamer a b => a -> a -> a
selectLtR xs ys = newStream (choose xs ys)
  where choose l r = if sEmpty l
                     then sFront r : choose l (sRest r)
                     else sFront l : choose (sRest l) r

{-|
   Duplicate a stream and create two new, independent streams with identical
   contents as the original.
-}
dup :: Streamer a b => a -> (a,a)
dup s = 
  let (l1,l2) = unzip (dupList s) 
  in (newStream l1, newStream l2)
  where 
    dupList st = let hd = sFront st 
                 in (hd,hd) : (dupList (sRest st))

{-|
   Split a stream into two.  If spitting an effectful stream, the effects
   will be seen by both recipients.  Use dup to avoid this if desired.
-}
split :: Streamer a b => a -> (a,a)
split s = (left,right)
  where left  = lifter id s
        right = lifter id s

{-|
   Alternator split
-}
altsplit :: (Streamer a b) => a -> (a,a)
altsplit s = 
  let (l1,l2) = unzip(altList s)
  in (newStream l1, newStream l2)
  where
    altList st = let hd = id $! sFront st in
                 let hd2 = id $! sFront (sRest st)
                 in (hd,hd2):(altList (sRest (sRest st)))

{-|
   Alternator merge
-}
altmerge :: (Streamer a b) => a -> a -> a
altmerge xs ys = newStream (alt xs ys)
  where alt l r = sFront l : (alt r (sRest l))

{-|
   zipper merge
-}
zipper :: (Streamer a b, Streamer a' b', Streamer c (b,b'))
       => a -> a' -> c
zipper xs ys = newStream (zipped xs ys)
  where zipped l r = (sFront l, sFront r) :
  	             (zipped (sRest l) (sRest r))

{-|
   Compose a sequence of activities together
-}
compose :: (Streamer a b, Streamer c d, Streamer e f)
        => (a->c) -> (c->e) -> (a->e)
compose f g = g . f

{-|
   A stream that forever provides a single constant.
-}
constantStream :: (Streamer a b) => b -> a
constantStream c = newStream (repeat c)

{-|
   Provide an initial list for the stream to start with and produce an
   activity that will consume a stream and emit a new stream with the
   initial list at the front of the stream passed in.  Note that if the
   seed list is infinite, the stream being passed in will never be 
   serviced, so beware.
-}
seededStream :: (Streamer a b) => [b] -> a -> a
seededStream l s = selectLtR (newStream l) s

{-|
   Produce a monotonically increasing counter with a starting value.
-}
counterStream :: (Enum b, Streamer a b) => b -> a
counterStream start = newStream ([start..])
