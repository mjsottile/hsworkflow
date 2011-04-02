{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}

module Main where

import Happstack.State
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent (MVar)

-- First we define our basic datatype.  In this case, it is simply
-- a wrapper around an Int.
data ExampleState = ExampleState Int
    deriving (Typeable, Show)

{- Here we accept the default for Version, which corresponds to a version number of 0
   and no previous versions of the state defined.
   You might be saying "Hold on, there, what's all that about?".
   I'm going to say "Oh look, a Unicorn!" and hope you remain distracted until the
   chapter on versions & migrations. -}

instance Version ExampleState

-- This is a bit of lovely Template Haskell hackery that you really
-- shouldn't think too hard about.  Suffice to say, you need to call this.
$(deriveSerialize ''ExampleState)

{- Now we're getting into the real meat of our application!
   The Update and Query methods that we can use to actually
   manipulate the persistent state. 

   The two important points are that Update st is a 
   MonadState st and that Query st is a MonadReader st.
   
   If you have any familiarity with the mtl library, you can
   apply all your normal intuition about Reader & State monads
   to writing updates and queries.
-}

-- A very simple Update that just increments the state
succVal :: Update ExampleState ()
succVal = modify (\(ExampleState n) -> ExampleState (n+1))

-- A simple Query that just returns the bare Int contained in the state
getVal :: Query ExampleState Int
getVal = do
  ExampleState n <- ask
  return n

-- An Update that actually takes an argument.  That may not seem odd
-- at the moment, but we'll talk more about it in a second.
incVal :: Int -> Update ExampleState ()
incVal i = modify (\(ExampleState n) -> ExampleState (n+i))

-- An Update that makes use of the utility function getRandomR.
-- getRandom or getRandomR can be used in the middle of an Update
-- or Query without needing to perform any unsafe IO magic or somehow
-- carry around a random seed with your state.
-- This Update also serves as a reminder that despite all the deep
-- magic that is used to make Updates and Querys work, these are
-- still legitimate Haskell monads and we can combine them together
-- the same as always.

succRand :: Update ExampleState ()
succRand = getRandomR (0,1000) >>= incVal

{- Another required bit of Template Haskell magic.  This will derive
   a set of data types to be used in the over all event handling that
   are in 1-1 correspondence with the Updates and Querys that you
   defined up above.  Try compiling this with the -ddump-splices option
   in order to actually take a look at what gets created by mkMethods.
   
   The data types will have the same name as your functions except
   with a capitalized first letter.

   The parameters to your Update or Query become the parameters to the
   constructor of your data type.  So in this case incVal becomes
   IncVal Int.
-}

$(mkMethods ''ExampleState ['succVal, 'getVal, 'incVal, 'succRand])

-- One more thing you can't forget to define in order for MACID to work
-- If your data type is composed from non-primitive data, then you need
-- to build up a type level list using the constructor :+: and the
-- empty dependency End.  Another example is going to cover the use of sub components 
-- and when you'd want a non-trivial Dependencies type.
instance Component ExampleState where
    type Dependencies ExampleState = End
    initialValue = ExampleState 0

{-
  The way you actually start running a Happstack.State
  application is to call startSystemState and pass it
  a Proxy.  As you can see below in the definition of
  rootState, the Proxy type carries no actual data,
  it is simply a carrier of the type of your state.
 
 startSystemState is the function you'll most often use
 for Happstack.State clients.  It uses the default
 behavior of serializing data to files under _local.
 While this isn't the only option for serialization of
 state, it is the only one we will consider in this
 tutorial.
-}
rootState :: Proxy ExampleState
rootState = Proxy

main :: IO ()
main = startSystemState rootState >>= commandLoop

commandLoop :: MVar TxControl -> IO ()
commandLoop ctl = do
  putStrLn "Enter 'i' to increment the state by a number of your choosing."
  putStrLn "Enter 'v' to view the state."
  putStrLn "Enter 's' to increment the state by 1."
  putStrLn "Enter 'c' to checkpoint the state."
  putStrLn "Enter 'r' to increment the state by a random number."
  putStrLn "Enter 'q' to quit."
  val <- liftM head getLine
  handler ctl val

{-
  At last, our handler function is quite simple and bare bones.
  If you experiment with it at the command line you should able
  to watch it work.  There isn't much in the way of a UI here,
  but it's enough that you can get the flavor of how one uses
  Happstack.State in the main application logic.

  The two functions I'd like to discuss are query and update.
  They operate fairly similarly to each other.

  update :: (MonadIO m, UpdateEvent ev res) => ev -> m res
  query :: (MonadIO m, QueryEvent ev res) => ev -> m res

  You, basically, pass data corresponding to Query functions
  to query and Updates to update.  That's really what the
  type context about UpdateEvent and QueryEvent comes down
  to.  The really nice thing is that they work for any
  MonadIO.  In the example below, we just use IO itself, but
  since ServerPartT has a MonadIO instance we can very
  seamlessly use update & query in our Happstack web apps.

  Now, we're explicitly threading through the MVar TxControl
  returned by our call to startSystemState into the handler.
  It'd probably be cuter to just use a ReaderT (MVar TxControl) IO
  but I want to keep this code extremely straight forward.
  The only place where we actually need this TxControl is when
  calling the createCheckpoint function.

  createCheckpoint does exactly what it sounds like.  It
  serializes a checkpoint of the application state.  
  You might be wondering what a checkpoint does if the state
  is already persistent.  Normally Happstack.State serializes
  the event log, which is played back whenever the application
  is restarted.  When you create a checkpoint, you are 
  serializing out a state you can use as a starting point
  for your application when it is restarted.  You might
  want to try the following outline to build your intuition
  for checkpoints.

  1. Increment the state a few times
  2. Create a checkpoint
  3. Increment the state again
  4. Close down the app and examine the
     _local directory.  You should see
     the files corresponding to the checkpoint
     and the events recorded after it
  5. Restart the app.  It should be right where
     you left it.  Close it again.
  6. Delete the events files
  7. Restart the app.  The state should correspond
     to the checkpoint you took.
  8. ???
  9. Profit!
-}

handler :: MVar TxControl -> Char -> IO ()
handler _ 'q' = return ()
handler c 'v' = query GetVal >>= print >> commandLoop c
handler c 's' = update SuccVal >> commandLoop c
handler c 'i' = flip catch (const $ commandLoop c) $ do
                v <- readLn
                update $ IncVal v
                commandLoop c
handler c 'c' = createCheckpoint c >> commandLoop c
handler c 'r' = update SuccRand >> commandLoop c
handler c _ = commandLoop c 