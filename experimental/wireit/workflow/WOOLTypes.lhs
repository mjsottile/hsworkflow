>   {-# OPTIONS_GHC -fglasgow-exts #-}

>   module WOOLTypes where

>   import Text.Show.Functions ()
>   import Data.Dynamic
>   import Control.Monad.State

>   type Output = [String]
>   type Debug = [String]


A Tag is some sort of unique id that I can use to associate loosely coupled
things such as the activity that produces an output and the activity that
consumes it.

>   type    PipeTag = String
>   type    Tag = PipeTag
>   type    OperatorID = Int
>   type    PortIndex = Int

>   data Signature = Signature (Int,Int)
>       deriving (Show)

========= CanFlow and Token ==

The CanFlow class encapsulates the behavior necessary for data tokens
stored within a pipe.

>   class CanFlow b where
>       isSignal    :: b -> Bool
>       isMissing   :: b -> Bool
>       isString    :: b -> Bool
>       isInt       :: b -> Bool
>       toList      :: b -> [Token]
>       toInt       :: b -> Int
>       toDouble    :: b -> Double
>       toString    :: b -> String
>       toChar      :: b -> Char
>       toPair      :: b -> (Token,Token)
>       toDynamic   :: b -> Dynamic



>   data Token = forall a . (CanFlow a,Show a) => ToToken a
>       deriving (Typeable)

>   instance CanFlow Token where
>       isSignal (ToToken a) = isSignal a
>       isMissing (ToToken a) = isMissing a
>       isString (ToToken a) = isString a
>       isInt (ToToken a) = isInt a
>       toList (ToToken a) = toList a
>       toInt (ToToken a) = toInt a
>       toDouble (ToToken a) = toDouble a
>       toString (ToToken a) = toString a
>       toChar (ToToken a) = toChar a
>       toPair (ToToken b) = toPair b
>       toDynamic (ToToken b) = toDynamic b

>   instance Show Token where
>       show (ToToken a) = show a


The Missing datatype/value is used to indicate that there is no
data available (on an input or output pipe/port). Missing is not intended
to serve as a 'database' missing or null value, although it could conceivably
do so.

>   data Missing = Missing
>       deriving (Show,Typeable)

>   instance CanFlow Missing where
>       isSignal _ = False
>       isMissing _ = True
>       isString _ = False
>       isInt _ = False
>       toList _ = []
>       toInt _ = (-1)::Int
>       toDouble _ = (-1.0)
>       toString _ = ""
>       toChar _ = '?'
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = toDyn b

>   toMissingToken :: Token
>   toMissingToken = ToToken Missing


The Signal datatype is mainly intended to be a valueless control signal,
usually to support traditional control flow.

>   data Signal = Signal
>       deriving (Show,Typeable)

>   instance CanFlow Signal where
>       isSignal _ = True
>       isMissing _ = False
>       isString _ = False
>       isInt _ = False
>       toList _ = []
>       toInt _ = (-2)::Int
>       toDouble _ = (-2.0)
>       toString _ = ""
>       toChar _ = '?'
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = toDyn b

>   toSignalToken :: Token
>   toSignalToken = ToToken Signal



Common scalar datatypes are flowable

>   instance CanFlow Int where
>       isSignal _ = False
>       isMissing _ = False
>       isString _ = False
>       isInt _ = True
>       toList _ = []
>       toInt i = i::Int
>       toDouble i = fromIntegral i
>       toString _ = ""
>       toChar _ = '?'
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = toDyn b

>   toIntToken :: Int -> Token
>   toIntToken a = ToToken a


>   instance CanFlow Double where
>       isSignal _ = False
>       isMissing _ = False
>       isString _ = False
>       isInt _ = False
>       toList _ = []
>       toInt d = round d
>       toDouble d = d::Double
>       toString _ = ""
>       toChar _ = '?'
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = toDyn b

>   toDoubleToken :: Double -> Token
>   toDoubleToken a = ToToken a




Here we specify that any array of flowables is a flowable.
It is unfortunate that we cannot yet use an array of arbitrary type
as a flowable.

>   instance (CanFlow a,Show a,Typeable a) => CanFlow [a] where
>       isSignal _ = False
>       isMissing _ = False
>       isString _ = False
>       isInt _ = False
>       toList b =
>           let
>               f x = ToToken x
>           in
>               map f b
>       toInt _ = (-777777)::Int
>       toDouble _ = (-3.0)
>       toString s =
>           let
>               f x = toChar x
>           in
>               map f s
>       toChar _ = '?'
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = toDyn b


>   toListToken :: [Token] -> Token
>   toListToken a = ToToken $ a


Pair Token

>   instance (Show a,Show b,CanFlow a,CanFlow b,Typeable a,Typeable b) =>
>       CanFlow (a,b) where
>       isSignal _ = False
>       isMissing _ = False
>       isString _ = False
>       isInt _ = False
>       toList _ = []
>       toInt _ = (-8)::Int
>       toDouble _ = (-8.0)
>       toString s = show s
>       toChar _ = '?'
>       toPair (a,b) = (ToToken a,ToToken b)
>       toDynamic b = toDyn b

>   toPairToken :: (Show a,Show b,CanFlow a,CanFlow b,Typeable a,Typeable b) => (a,b) -> Token
>   toPairToken a = ToToken $ a


>   instance CanFlow Char where
>       isSignal _ = False
>       isMissing _ = False
>       isString _ = True
>       isInt _ = False
>       toList _ = []
>       toInt _ = (-4)::Int
>       toDouble _ = (-9.0)
>       toString c = [c]
>       toChar c = c
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = toDyn b


>   toStringToken :: String -> Token
>   toStringToken a = ToToken a

>   makeStringTokenList   ::  [String] -> [Token]
>   makeStringTokenList   [] = []
>   makeStringTokenList   (s:ss) = (toStringToken s):(makeStringTokenList ss)


>   instance CanFlow Dynamic where
>       isSignal _ = False
>       isMissing _ = False
>       isString _ = False
>       isInt _ = False
>       toList _ = []
>       toInt _ = (-4)::Int
>       toDouble _ = (-9.0)
>       toString _ = ""
>       toChar _ = '?'
>       toPair _ = (toMissingToken,toMissingToken)
>       toDynamic b = b


========= Pipe ==

The Pipe is logically a deque, although many buffering and streaming
possibilities suggest themselves. The Pipe is manipulated via the push and
pull methods, which modify the pipe within the monad.

>   data    Pipe =  Pipe
>                   {
>                       getPipeTag      ::  PipeTag,
>                       getPipeData     ::  [Token]
>                   }
>       deriving ( Show )



========= Operator, CanRun and Runnable ==


The CanRun class has a run method that executes one 'tick' of behavior on
behalf of its operator. This is the means whereby HaskWOOL's engine is extended.


>   class (Monad m) => CanRun m b where
>        run :: OperatorID -> [PipeTag] -> [PipeTag] -> b -> WorkflowT m b


The Runnable wrapper class is used so that we can have lists of diverse
values that are all in the CanRun class.

>   data    Runnable m = forall b. (Monad m,CanRun m b,Show b) => Runnable b

>   instance (Monad m) => Show (Runnable m) where
>       show (Runnable b) = show b


The HaskWOOL workflow consists of Operators and Pipes.

Each operator's behavior is customized via the operator's Runnable core.

An operator corresponds to a Petri Net transition.

>   data    Operator m = (Monad m) =>
>               Operator
>               {
>                   getOperatorID           ::  OperatorID,
>                   getOperatorInputs       ::  [PipeTag],
>                   getOperatorOutputs      ::  [PipeTag],
>                   getOperatorRunnable     ::  Runnable m
>               }





========= WFState ==

WFState is the data bundle for our Workflow runtime. It is parameterized
by a monad type, to allow activities to operate within a desired monad
like the IO monad, but to keep the WOOL infrastructure pure of IO.

>   data WFState m = (Monad m) => WFState {
>                                   getDebug    :: Debug,
>                                   getTime     :: Int,
>                                   getId       :: OperatorID,
>                                   getPipes    :: [Pipe],
>                                   getOps      :: [Operator m],
>                                   getOutPipes :: [PipeTag],
>                                   getInPipes  :: [PipeTag]
>                               }

>   formatPipes :: [Tag] -> String
>   formatPipes [] = ""
>   formatPipes ( ptag:ps ) =
>       ( " {" ++ ptag ++ "}" ) ++ ( formatPipes ps )

>   instance (Monad m) => Show (Operator m) where
>       show ( o ) = "Operator[" ++ (show (getOperatorID o)) ++ "] " ++
>                       ( show (getOperatorRunnable o) ) ++ " .... " ++
>                       ( formatPipes (getOperatorOutputs o) ) ++ " <== " ++
>                       ( formatPipes (getOperatorInputs o) )

>   instance (Monad m) => Show (WFState m) where
>       show st = "WFState" ++
>                   "debug: " ++ (show (getDebug st) ) ++
>                   "time:  " ++ (show ( getTime st ) ) ++
>                   "id:    " ++ (show ( getId st ) ) ++
>                   "debug: " ++ (show ( getDebug st ) ) ++
>                   "pipes: " ++ (show ( getPipes st ) ) ++
>                   "ops:   " ++ (show ( getOps st ) ) ++
>                   "outs:  " ++ (show ( getOutPipes st ) ) ++
>                   "ins:   " ++ (show ( getInPipes st ) )

>   defaultInitialState :: (Monad m) => WFState m
>   defaultInitialState =
>                         WFState {
>                              getDebug = [],
>                              getTime = 0,
>                              getId = 1,
>                              getPipes = [],
>                              getInPipes = [],
>                              getOutPipes = [],
>                              getOps = []
>                          }

========= MControlT and MControl ==

MControl is the GADT that encodes a success as Return or a failure as
Fail. MControlT (see below) applies its inner monad to (MControl a),
rather than simply (a).

>   type Exception = String
>   data MControl a =
>           Fail Exception
>           |
>           Return
>           {
>               getValue :: a
>           }


MControlT is the middle monad transformer, which is currently a simple
exception monad. The StateT monad transformer is wrapped around MControlT
such that the important operations (bind,return,fmap) of MControlT are
invoked correctly when the corresponding operations of StateT are invoked.
Similarly, MControlT invokes the monad operations on its inner monad, which
is typically specified as the parameter to WorkflowT.

Potential Future applications of MControlT include:
    Return a value
    Fail with an exception.
    Promise a future... NYI
    Checkpoint the current computation NYI
    Result a checkpointed computation NYI
    Convert into various other datatypes/arities NYI

>   newtype MControlT m a = MControlT { runMControlT :: m (MControl a) }

>   instance (Monad m) => Monad (MControlT m) where
>       return a = MControlT $ return $ Return a
>       m >>= f = MControlT $ do
>               x <- runMControlT m
>               case x of
>                   Fail e -> return $ Fail e
>                   Return a -> runMControlT (f a)


Make our MControlT monad smell like a proper monad transformer.
The lift method converts an expression in the inner monad into
an MControl value.

>   instance MonadTrans MControlT where
>       lift x = MControlT $ do a <- x; return $ Return a


The liftIO method converts an inner MonadIO expression into
a suitable MControl value. Basically, it delegates liftIO to the
inner MonadIO expression.

>   instance (MonadIO m) => MonadIO (MControlT m) where
>       liftIO = lift . liftIO


========= WorkflowT ==

The WorkflowT monad transformer is really just a type synonym for a
suitably parameterized StateT transformer. This type constructor will
typically be used in the signature of a function. For example:
    evalT :: (Monad m) => String -> WorkflowT m Int
is a function that takes a string and returns an Int within the
WorkflowT m monad.

>   type WorkflowT m = StateT (WFState m) (MControlT m)

