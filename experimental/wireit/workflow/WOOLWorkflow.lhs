>   {-# OPTIONS_GHC -fglasgow-exts #-}
>   {-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
>   {-# OPTIONS_GHC -fno-warn-orphans #-}

This module is where the plumbing that ties together Pipes and Operators
lives. This includes:
    push/pull operations on Pipe
    start/run/step of workflows
    The Subflow Operator


>   module WOOLWorkflow where
    
>   import Text.Show.Functions ()
>   import Control.Monad.State

>   import WOOLTypes
>   import WOOLOperators
>   import WOOLUtil


=============== Workflow ==

This function is a kluge that should properly be respecting a Signature
argument. Currently, it is used to dynamically add pipes to a one-shot
workflow or subflow. This strategy will fail if we want to implement
resumable workflows and subflows.


>   addInputPipes :: (Monad m,MonadIO m) => (WorkflowT m b) -> [Token] -> (WorkflowT m b)
>   addInputPipes flow args =
>       let
>           makeInPipes :: (Monad m,MonadIO m) => [Token] -> Int -> WorkflowT m ()
>           makeInPipes [] _ = return ()
>           makeInPipes (d:ds) n = do
>               makeInPipe ( "thisOperator.In" ++ (show n) ) [d]
>               makeInPipes ds (n+1)
>       in do
>           makeInPipes args 0
>           flow 


>   addSignaturePipes :: (Monad m,MonadIO m) => (WorkflowT m b) -> Signature -> (WorkflowT m b)
>   addSignaturePipes flow ( Signature ( numIns, numOuts ) ) =
>       let
>           makeInPipes :: (Monad m,MonadIO m) => Int -> Int -> WorkflowT m ()
>           makeInPipes n maxn = do
>               _ <-    if n >= maxn then
>                           return ()
>                       else do
>                           makeInPipe ( "thisOperator.In" ++ (show n) ) []
>                           makeInPipes (n+1) maxn
>               return ()
>           makeOutPipes :: (Monad m,MonadIO m) => Int -> Int -> WorkflowT m ()
>           makeOutPipes n maxn = do
>               _ <-    if n >= maxn then
>                           return ()
>                       else do
>                           makeOutPipe ( "thisOperator.Out" ++ (show n) )
>                           makeOutPipes (n+1) maxn
>               return ()
>       in do
>           makeInPipes 0 numIns
>           makeOutPipes 0 numOuts
>           flow 




The newWorkflow function amends a workflow building function with code
to dynamically add input pipes based upon a Signature argument.

>   --  WorkflowT m () -> [Token] -> m (Maybe (WFState m))
>   newWorkflow :: (MonadIO m) => ( WorkflowT m () ) -> Signature -> m (Maybe (WFState m))
>   newWorkflow wf sig =
>       let
>       in
>           startWorkflow (addSignaturePipes wf sig)



=============== Workflow ==

The Workflow operator is used to embed a workflow within another workflow.
The inputs to the Workflow operator are currently passed as a [Token]
to the embedded workflow at workflow construction time, and a new
embedded instance is created for each new input case.

>   data    Workflow m = (Monad m) =>
>                       Workflow{
>                           workflowFactory :: WorkflowT m (),
>                           workflowWFState :: WFState m
>                       }

>   instance (Monad m) => Show (Workflow m) where
>       show _ = "Workflow {}"


makeWorkflow makes the assumption that the factory creates a workflow
with preexisting Input/Output pipes declared. makeWorkflow will not
auto-wrap the factory with pipe creation code.

>   makeWorkflow :: (Monad m,MonadIO m) => WorkflowT m () -> [Tag] -> [Tag] -> (WorkflowT m OperatorID)
>   makeWorkflow factory intags outtags =
>       let
>       in do
>           liftIO $ putStrLn $ "makeWorkflow"
>           (Just s0) <- startWorkflowInWorkflow factory
>           let act = Runnable $ Workflow{
>                                   workflowFactory=factory,
>                                   workflowWFState=s0
>                               }
>           result <- installActivity act intags outtags
>           liftIO $ putStrLn $ "makeWorkflow2" ++ (show s0)
>           liftIO $ WOOLUtil.printWorkflowActivities s0
>           liftIO $ WOOLUtil.printWorkflowPipes s0
>           liftIO $ WOOLUtil.printWorkflowDebug s0
>           return result


a wfmaker assumes it is run within an existing (possibly initial)
wfstate, which it amends with its installPipe/Operator calls.
Macro-style WF inclusion is easy, because wfmakers are really just wfAugmenters.

To make a resumable workflow, we need to:
- Ensure (at install time?) that the necessary IO pipes are in the workflow
- Ensure (at case input time) that the data from the external pipes are
    consumed and moved into the subflow's input pipes.
- Ensure that the workflow executes until it outputs something.
- The workflow should be resumable, and input should be addable in between
resumptions.

The newWorkflowOperator function inserts a Workflow operator and ensures that it
uses addSignaturePipes to augment the workflow with input/output pipes.
Such a workflow is resumable. Each resumption may (optionally) add values
to the input pipes.

>   newWorkflowOperator :: (Monad m,MonadIO m) => (WorkflowT m ()) -> Signature -> (WorkflowT m OperatorID)
>   newWorkflowOperator factory sig =
>       let
>           augmentedFactory = addSignaturePipes factory sig
>       in do
>           (Just s0) <- startWorkflowInWorkflow augmentedFactory
>           let act = Runnable $ Workflow{
>                                   workflowFactory=augmentedFactory,
>                                   workflowWFState=s0
>                               }
>           result <- installOperator act sig
>           liftIO $ WOOLUtil.printWorkflowActivities s0
>           liftIO $ WOOLUtil.printWorkflowPipes s0
>           liftIO $ WOOLUtil.printWorkflowDebug s0
>           return result


>   instance (Monad m,MonadIO m) => CanRun m (Workflow m) where
>       run oid intags outtags act@Workflow{
>                       workflowFactory = factory,
>                       workflowWFState = wfstate } =
>           let
>               writeResults :: (Monad m) => [Tag] -> [Token] -> WorkflowT m ()
>               writeResults [] _ = return ()
>               writeResults _ [] = return ()
>               writeResults (t:ts) (v:vs) = do
>                   _ <-    if ( isMissing v ) then do
>                               return ()
>                           else do
>                               push t $! v
>                   writeResults ts $ vs
>           in do
>               _ <-    if ((length intags) == 0) then do
>                           debugT $ "Workflow AutoFiring: " ++ (show act)
>                           (sn, results) <- proceedWorkflowInWorkflow wfstate ([]::[Token])
>                           debugTBatch (getDebug sn)
>                           writeResults outtags results
>                           let newop = Runnable $ Workflow{
>                               workflowFactory = factory,
>                               workflowWFState = sn
>                           }
>                           updateActivity oid newop
>                           return ()
>                       else do
>                           args <- pullAND intags
>                           _ <-    if ( ( length args ) > 0 ) then do
>                                       debugT $ "Workflow Firing: " ++ (show act) ++ " args=" ++ (show args)
>                                       (sn, results) <- proceedWorkflowInWorkflow wfstate args
>                                       let newop = Runnable $ Workflow{
>                                           workflowFactory = factory,
>                                           workflowWFState = sn
>                                       }
                                        
>                                       debugTBatch (getDebug sn)
>                                       debugT $ "writeResults: " ++ (show outtags) ++ " << " ++ (show results)
>                                       writeResults outtags results
>                                       debugT $ "returned from Workflow. results=" ++ 
>                                           (show outtags) ++ " << " ++ (show results)
>                                       updateActivity oid newop
>                                       return ()
>                                    else do
>                                       return ()
>                           return ()
>               return act



========= Scheduling =========

>   runWorkflowNCycles :: (Num t, MonadIO m) => WFState IO -> t -> m (WFState IO)
>   runWorkflowNCycles startState numCycles =
>       let
>           runN :: (Monad m,MonadIO m,Num t) => t -> WorkflowT m ()
>           runN 0 = return ()
>           runN n =
>               do
>                   incTtimer
>                   st <- get
>                   let as = getOps st
>                   runall as
>                   runN $ n - 1
>       in do
>           let runAction = runStateT (runN numCycles)
>           let ranControl = runAction startState
>           let ranIO = runMControlT ranControl
>           r <- liftIO ranIO
>           return $ case ( r ) of
>                       Fail _ ->
>                           startState
>                       Return ( _, endState ) ->
>                           endState


============== Workflow Functions ==========

The exportWorkflowState function converts an MControl monad into a Maybe
value containing either Nothing (if the MControl contained an error), or
(Just wfstate), where wfstate is a complete WFState for the workflow.

This wfstate can be examined via the getWorkflowXXX methods.

>   exportWorkflowState :: (Monad m) => (MControl ( (), WFState m )) -> (Maybe (WFState m))
>   exportWorkflowState wfmonad =
>       case ( wfmonad ) of
>           Fail _ ->
>               Nothing
>           Return ( _, wfstate ) ->
>               Just wfstate



The startWorkflow function is the primary means for a client to initiate
the execution of a workflow. The parameter is a WorkflowT that defines an
initial set of activities and pipes, possibly initializing them to some
known state. startWorkflow will construct an initial state and then
transform it with runStateT in conjunction with the wfmaker parameter.

The result is a new state (wrapped in an MControlT and IO monad) that
represents the system after the addition of the pipes/activities from
wfmaker.


>   startWorkflow :: (Monad m) => (WorkflowT m ()) -> m (Maybe (WFState m))
>   startWorkflow wfmaker =
>       let
>           wf = startWorkflowAux wfmaker
>       in do
>           r <- wf
>           let (Just rr) = exportWorkflowState r
>           return $ Just rr


The startWorkflowInWorkflow method is similar, except that the result
is delivered to the WorkflowT monad, rather than the outer monad.

>   startWorkflowInWorkflow :: (Monad m,MonadIO m) => (WorkflowT m ()) -> WorkflowT m (Maybe (WFState m))
>   startWorkflowInWorkflow wfmaker =
>       let
>           wf = startWorkflowAux wfmaker
>       in do
>           r <- lift $ lift wf
>           return $ exportWorkflowState r


>   startWorkflowAux :: (Monad m) => (WorkflowT m ()) -> (m (MControl ((), WFState m)))
>   startWorkflowAux wfmaker =
>       let
>           initialState = defaultInitialState
>           initAction = runStateT wfmaker
>           initedControl = initAction initialState
>           initedInner = runMControlT initedControl
>       in
>           initedInner



This is (or will be) the primary way to invoke a workflow and run it
to 'completion'. Completion is achieved when the workflow's output
pipe gets a value.

>   proceedWorkflow :: (Monad m,MonadIO m) =>
>                       (WFState m) -> [Token] -> m ((WFState m),[Token])
>   proceedWorkflow startState args =
>       let
>           wf = proceedWorkflowWithArgsAux startState args
>       in do
>           r <- wf
>           return $ case ( r ) of
>                       Fail _ ->
>                           ( startState, [] )
>                       Return ( (Just output), endState ) ->
>                           ( endState, output )
>                       Return ( Nothing, endState ) ->
>                           ( endState, [] )


>   proceedWorkflowInWorkflow :: (Monad m,MonadIO m) => (WFState m) -> [Token] -> WorkflowT m ((WFState m),[Token])
>   proceedWorkflowInWorkflow startState args =
>       let
>           wf = proceedWorkflowWithArgsAux startState args
>       in do
>           r <- lift $ lift wf
>           return $ case ( r ) of
>                       Fail _ ->
>                           ( startState, [] )
>                       Return ( (Just output), endState ) ->
>                           ( endState, output )
>                       Return ( Nothing, endState ) ->
>                           ( endState, [] )


>   proceedWorkflowWithArgsAux :: (Monad m,MonadIO m) => (WFState m) -> [Token] -> m (MControl (Maybe [Token], WFState m))
>   proceedWorkflowWithArgsAux startState args =
>       let
>           runLoop = do
>               debugT $ ""
>               incTtimer
>               st <- get
>               let as = getOps st
>               runall as
>               st2 <- get
>               let outpipetags = getOutPipes st2
>               let (vs,smallerPipes,nValues) = pullEachPipe (getPipes st2) outpipetags [] [] 0
>               
>               -- If none of the outputs are present, then we run again
>               -- liftIO $ putStrLn $ "runWorkflowAux...outpipetags=" ++ (show outpipetags) ++ " nvalues=" ++ (show nValues)
>               -- liftIO $ putStrLn $ "..." ++ (show vs)
>               y <-    if ( nValues == 0 ) then do
>                           -- This may be a stack-overflowing, non-tail recursive loop
>                           -- Must fix it to be tail-recursive.
>                           x <- runLoop
>                           return x
>                       else do
>                           let newSt = WFState{
>                                                   getDebug = (getDebug st2),
>                                                   getTime = (getTime st2),
>                                                   getId = (getId st2),
>                                                   getPipes = smallerPipes,
>                                                   getInPipes = (getInPipes st2),
>                                                   getOutPipes = (getOutPipes st2),
>                                                   getOps = (getOps st2)
>                                               }
>                           put $ newSt
>                           return $ Just vs
>               return y

>           injectArgs _ [] = do { return (); }
>           injectArgs [] _ = do { return (); }
>           injectArgs (p:ps) (v:vs) = do
>               push p v
>               injectArgs ps vs
>               
>           runProgram = do
>               st <- get
>               let inpipetags = getInPipes st
>               injectArgs inpipetags args
>               runLoop
>
>           runAction = runStateT (runProgram)
>           ranControl = runAction startState
>           ranInner = runMControlT ranControl
>       in
>           ranInner


When I get some time...
The stepWorkflow and proceedWorkflowWithArgsAux functions are similar in
that they take a function > that executes within the workflow and use it as
a state transformer.
A single general-purpose function could be used for both of these, as well
as runNCycles. Basically, we can generalize the concept of 'proceed' to
include initialization,procession,and termination conditions.


The stepWorkflow function takes an initial WFState and performs one
cycle, which means every WOOL thread will get ticked once. The result
is the modified WFState corresponding to the post-cycle system.

>   stepWorkflow :: (Monad m,MonadIO m) => (WFState m) -> m (Maybe (WFState m))
>   stepWorkflow initialState =
>       let
>           step :: (Monad m,MonadIO m) => WorkflowT m ()
>           step =
>               do
>                   incTtimer
>                   st <- get
>                   let as = getOps st
>                   runall as
>       in do
>           let runAction = runStateT step
>           let ranControl = runAction initialState
>           let ranInner = runMControlT ranControl
>           r <- ranInner
>           let (Just rr) = exportWorkflowState r
>           return $ Just rr


This stopExecT method injects a Fail state into the monad transformer by
using lift in conjunction with the inner Fail injection.
This feature is not currently used, and brings the entire workflow to an
abrupt halt, losing any intermediate information.

>   stopExecT :: (Monad m) => String -> WorkflowT m ()
>   stopExecT e = lift $ MControlT $
>       do
>           -- liftIO $ putStrLn $ "stopExecT " ++ e
>           return $ Fail e


Increment the timer without changing anything else.

>   incTtimer :: (Monad m) => WorkflowT m ()
>   incTtimer =
>       do
>           st <- get
>           let s = getTime st
>           let newSt =     WFState{
>                               getDebug = (getDebug st),
>                               getTime = (s+1),
>                               getId = (getId st),
>                               getPipes = (getPipes st),
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = (getOps st)
>                           }
>           put $ newSt


uninstallOperator removes the specified thread from the list of active threads.
If the opId does not exist, then there will be no effect.

>   uninstallOperator :: (Monad m) => OperatorID -> WorkflowT m ()
>   uninstallOperator opId =
>       let
>           removeOpFromList _ [] result = result
>           removeOpFromList lookforid (o:os) result =
>               removeOpFromList lookforid os $
>                   result ++ ( if lookforid == (getOperatorID o) then [] else [ o ] )
>       in do
>           debugT $ "opId - " ++ (show opId)
>           st <- get
>           let os = getOps st
>           let newts = removeOpFromList opId os []
>           let newSt =     WFState{
>                                       getDebug = (getDebug st),
>                                       getTime = (getTime st),
>                                       getId = (getId st),
>                                       getPipes = (getPipes st),
>                                       getInPipes = (getInPipes st),
>                                       getOutPipes = (getOutPipes st),
>                                       getOps = newts
>                                   }
>           put $ newSt
>           return ()





The runall function just calls run1 on each thread in the WFState.
There is probably a monadic equivalent of map that would simplify this code,
but it's only a few lines, so I haven't looked it up yet.

>   runall :: (Monad m,MonadIO m) => [Operator m] -> WorkflowT m ()
>   runall [] = return ()
>   runall (olda:as) =
>       let
>           run1 :: (Monad m,MonadIO m) => (Operator m) -> WorkflowT m ()
>           run1 ( Operator
>                       {
>                           getOperatorID=opID,
>                           getOperatorInputs=ins,
>                           getOperatorOutputs=outs,
>                           getOperatorRunnable=(Runnable r)
>                       } ) =
>               do
>                   run opID (ins) (outs) r
>                   return ()
>       in do
>           run1 olda
>           runall as            
>           return ()

