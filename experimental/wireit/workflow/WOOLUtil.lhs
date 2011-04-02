>   {-# OPTIONS_GHC -fglasgow-exts #-}

(This is literate Haskell code; suffix file with .lhs)

This file is not required in order to use the WOOL engine.

It contains the legacy methods runwf/runwfv.

It contains various debugging and printing functions.

>   module WOOLUtil where

>   import IO
>   import WOOLTypes
>   import WOOLOperators()
>   import Control.Monad.State


Utility function to print a list of Pipe.

>   printPipes :: [Pipe] -> IO ()
>   printPipes [] = return ()
>   printPipes (p:ps) =
>       do
>           let (Pipe{ getPipeTag=ptag, getPipeData=pdata }) = p
>           putStrLn $ "Pipe[" ++ (ptag) ++ "] " ++ (show pdata)
>           printPipes ps


Utility function to print a list of Activity.

>   printActs :: (Show a) => [a] -> IO ()
>   printActs [] = do
>                       return ()
>   printActs (a:as) =
>       do
>           putStrLn $ (show a)
>           printActs as


Utility function to print a list of strings.

>   printAll :: [String] -> IO ()
>   printAll [] = return ()
>   printAll (a:xs) =
>       do
>           putStrLn a
>           printAll xs

= Test Driver. Pass it a function ( () -> WorkflowT m a ) =

>   printData :: (Show a) => [a] -> IO ()
>   printData [] = return ()
>   printData (x:xs) =
>       do
>           putStrLn $ (show x)
>           printData xs


The type signature for runwf and runwfv is currently not specified
because I can't figure out a valid type signature.


>   runwf :: (Show a, Monad m) =>
>               (() -> StateT (WFState m) (MControlT IO) [a]) -> IO ()
>   runwf wfmaker =
>       do
>           let wf = wfmaker ()
>           let ss = runStateT wf defaultInitialState
>           let rr = runMControlT ss
>           r <- liftIO rr
>           case ( r ) of
>               Fail e ->
>                   do
>                       putStrLn e
>                       return ()
>               Return (d, _) ->
>                   do
>                       printData d
>                       return ()
>           return ()


runwfv is the same as runwf, but it is more verbose.
It prints out the debug and output, as well as the state of the activities
and pipes after execution.

>   runwfv :: (Show a, Monad m) => (() -> StateT (WFState m) (MControlT IO) [a]) -> IO ()
>   runwfv wfmaker =
>       do
>           let wf = wfmaker ()
>           let s = runStateT wf defaultInitialState
>           r <- runMControlT s
>           case ( r ) of
>               Fail e ->
>                   do
>                       putStrLn $ "Fail: " ++ e
>                       return ()
>               Return ( v, st ) ->
>                   do
>                       putStrLn "====== Results ======="
>                       printData v
>                       putStrLn "====== Debug ======="
>                       case (getDebug st) of
>                         []    -> putStrLn $ "Ticks: " ++ (show (getTime st))
>                         _     -> do
>                                   printAll $ reverse (getDebug st)
>                                   putStrLn $ "Ticks: " ++ (show (getTime st))
>                       putStrLn "\n====== Pipes ======="
>                       printPipes $ getPipes st
>                       putStrLn "\n====== Acts ======"
>                       printActs $ getOps st
>                       return ()
>       

>   printWorkflowActivities :: (Monad m) => WFState m -> IO ()
>   printWorkflowActivities st =
>           do
>               putStrLn "\n====== Activities ======="
>               printActs $ getOps st
>               return ()


>   printWorkflowPipes :: (Monad m) => WFState m -> IO ()
>   printWorkflowPipes st =
>           do
>               putStrLn "\n====== Pipes ======="
>               printPipes $ getPipes st
>               return ()


>   printWorkflowDebug :: WFState m -> IO ()
>   printWorkflowDebug st =
>           do
>               putStrLn "\n====== Debug ======="
>               case (getDebug st) of
>                 []    -> putStrLn $ "Ticks: " ++ (show (getTime st))
>                 _     -> do
>                           printAll $ reverse (getDebug st)
>                           putStrLn $ "Ticks: " ++ (show (getTime st))
>               return ()


>   printWorkflow :: (Monad m) => WFState m -> IO ()
>   printWorkflow st =
>           do
>               putStrLn "====== Debug ======="
>               case (getDebug st) of
>                 []    -> putStrLn $ "Ticks: " ++ (show (getTime st))
>                 _     -> do
>                           printAll $ reverse (getDebug st)
>                           putStrLn $ "Ticks: " ++ (show (getTime st))
>               putStrLn "\n====== Pipes ======="
>               printPipes $ getPipes st
>               putStrLn "\n====== Acts ======"
>               printActs $ getOps st
>               return ()

