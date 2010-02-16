{-|
   module of workflow types based on streams
  
   matt\@cs.uoregon.edu
   ghulette\@cs.uoregon.edu
-}

module WF.Types where

import Control.Concurrent.MVar
import Maybe
import System.IO.Unsafe

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

{-|
   Type class representing generic stream operations.  The intent of the
   type class is so we can have instances for both Streams and EStreams
   allowing code to be written generically for both using the same
   basic operations.
-}
class Streamer s where
  newStream :: [a] -> s a
  sAdvanceN :: Int -> s a -> ([a],s a)
  sEmpty    :: s a -> Bool
  sAdvance  :: s a -> (a, s a)


{-|
   Pure stream.  No side effects seen by other activities when one
   activity operates on it.
-}
type PStream a = [a]


{-| 
   Instance for pure streams
-}
instance Streamer [] where
  newStream l   = l
  sAdvanceN i s = (take i s, drop i s)
  sEmpty        = null
  sAdvance s    = (head s, tail s)


{-|
   Effectful stream.  When an activity takes the head from the stream,
   this has an effect on the stream such that no other activity will see
   that element as the head.
-}
data EStream a = EStream (MVar [a])

{-|
   Instance for effectful streams
-}
instance Streamer EStream where
  newStream l = unsafePerformIO $ do
    m <- newMVar l
    return $ EStream m
  
  sAdvanceN n s = unsafePerformIO $ do 
    let EStream m = s
    r <- modifyMVar m (\i -> return $ swap $ sAdvanceN n i)
    return $ (r, s)

  sEmpty s = unsafePerformIO $ do
    let EStream m = s
    withMVar m (\i -> return $ sEmpty i)

  sAdvance s = unsafePerformIO $ do 
    let EStream m = s
    r <- modifyMVar m (\i -> return $ swap $ sAdvance i)
    return $ (r, s)

