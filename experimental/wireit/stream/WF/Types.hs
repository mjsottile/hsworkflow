{-|
   module of workflow types based on streams
  
   matt\@cs.uoregon.edu
-}

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module WF.Types where

import Control.Concurrent.MVar
import System.IO.Unsafe
import Maybe

{-|
   Pure stream.  No side effects seen by other activities when one
   activity operates on it.
-}
type PStream a = [a]

{-|
   Effectful stream.  When an activity takes the head from the stream,
   this has an effect on the stream such that no other activity will see
   that element as the head.
-}
type EStream a = MVar (PStream a)

{-|
   Type class representing generic stream operations.  The intent of the
   type class is so we can have instances for both Streams and EStreams
   allowing code to be written generically for both using the same
   basic operations.
-}
class Streamer a b | a -> b where
  newStream :: [b] -> a
  sFront    :: a -> b
  sRest     :: a -> a
  sTake     :: Int -> a -> [b]
  sEmpty    :: a -> Bool

{-| 
   Instance for pure streams
-}
instance Streamer (PStream a) a where
  newStream l = l
  sFront      = head
  sRest       = tail
  sTake       = take
  sEmpty      = null

{-|
   Instance for effectful streams
-}
instance Streamer (EStream a) a where
  newStream l = 
    unsafePerformIO (do store <- newEmptyMVar
                        putMVar store l
                        return store)
  
  sFront   s   = 
    unsafePerformIO (do stream <- takeMVar s
                        putMVar s $ sRest stream
                        return $ sFront stream)

  sRest   s   = s

  sTake n s =
    unsafePerformIO (do stream <- takeMVar s
                        putMVar s $ drop n stream
                        return $ take n stream)

  sEmpty s = 
    unsafePerformIO (do stream <- readMVar s
                        return (sEmpty stream))

-- HACK: These are added to make the GUI not have to specify types
toPStream :: PStream a -> PStream a
toPStream = id

toEStream :: EStream a -> EStream a
toEStream = id
