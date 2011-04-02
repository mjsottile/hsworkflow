>   {-# OPTIONS_GHC -fglasgow-exts #-}
>   {-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
>   {-# OPTIONS_GHC -fno-warn-orphans #-}

>   module WOOLOperators where

>   import Text.Show.Functions ()
>   import Control.Monad.State
>   import WOOLTypes


================== Pipe creation and manipulation ===============
    
installPipe will add the given pipe to the current workflow. If an existing
pipe with the same name exists, the pipe is NOT overridden with the new pipe.
Note: An equally valid spec is to have the new pipe always override, to issue
an error, or even to allow duplicates.

>   installPipe :: (Monad m) => Tag -> [Token] -> WorkflowT m ()
>   installPipe newtag newcontents =
>       let
>           addPipe [] = [Pipe{ getPipeTag=newtag, getPipeData=newcontents }]
>           addPipe ( ( p@( Pipe{ getPipeTag=tag, getPipeData=_ } ) ):ps) =
>               if tag == newtag then
>                   p:ps
>               else
>                   p:( addPipe ps)
>       in do
>           st <- get
>           let ps = getPipes st
>           let newps = addPipe ps
>           let newSt =     WFState{
>                               getDebug = (getDebug st),
>                               getTime = (getTime st),
>                               getId = (getId st),
>                               getPipes = newps,
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = (getOps st)
>                           }
>           put $ newSt


>   makePipe :: (Monad m) => Tag -> WorkflowT m ()
>   makePipe tag =
>       do
>           installPipe tag []


>   addTag :: [Tag] -> Tag -> [Tag]
>   addTag [] t = [t]
>   addTag ( t:ts ) newtag =
>       if t == newtag then
>           t:ts
>       else
>           t:( addTag ts newtag)


>   makeInPipe :: (Monad m) => Tag -> [Token] -> WorkflowT m ()
>   makeInPipe tag d =
>       let
>       in do
>           installPipe tag d
>           st <- get
>           let newinpipes = addTag (getInPipes st) tag
>           let newSt = WFState{
>                           getDebug = (getDebug st),
>                           getTime = (getTime st),
>                           getId = (getId st),
>                           getPipes = (getPipes st),
>                           getOps = (getOps st),
>                           getOutPipes = (getOutPipes st),
>                           getInPipes = newinpipes
>                       }
>           put $ newSt
>           return ()


>   makeOutPipe :: (Monad m,MonadIO m) => Tag -> WorkflowT m ()
>   makeOutPipe tag =
>       let
>       in do
>           makePipe tag
>           st <- get
>           let newoutpipes = addTag (getOutPipes st) tag
>           let newSt = WFState{
>                           getDebug = (getDebug st),
>                           getTime = (getTime st),
>                           getId = (getId st),
>                           getPipes = (getPipes st),
>                           getOps = (getOps st),
>                           getOutPipes = newoutpipes,
>                           getInPipes = (getInPipes st)
>                       }
>           put $ newSt
>           return ()


>   pushMatchingPipe :: [Pipe] -> Tag -> Token -> [Pipe]
>   pushMatchingPipe [] _ _ = []
>   pushMatchingPipe (p:ps) tag val =
>       let
>           ptag = getPipeTag p
>       in
>           if ptag == tag then
>               let
>                   newdata = (getPipeData p) ++ [val]
>                   newp = p{getPipeData=newdata}
>               in
>                   newp:ps
>           else
>               p:(pushMatchingPipe ps tag val)


>   push :: (Monad m,MonadIO m) => Tag -> Token -> WorkflowT m ()
>   push tag n =
>       do
>           -- liftIO $ putStrLn $ " " ++ (tag) ++ ".push <<< " ++ (take 30 (show n))
>           st <- get
>           let newps = pushMatchingPipe (getPipes st) tag n
>           let newSt =     WFState{
>                               getDebug = (getDebug st),
>                               getTime = (getTime st),
>                               getId = (getId st),
>                               getPipes = newps,
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = ( getOps st )
>                           }
>           put $ newSt


>   pullMatchingPipe :: [Pipe] -> Tag -> (Token,[Pipe])
>   pullMatchingPipe [] _ = (toMissingToken,[])
>   pullMatchingPipe (p:ps) tag =
>       if (getPipeTag p) == tag
>       then
>           case (getPipeData p) of
>               [] -> (toMissingToken, p:ps)
>               (hd:rest) -> (hd, p{ getPipeData=rest }:ps)
>       else
>           let
>               (result, newps ) = pullMatchingPipe ps tag
>           in
>               (result, p:newps)


>   pullEachPipe :: [Pipe] -> [Tag] -> [Token] -> [Pipe] -> Int -> ([Token],[Pipe],Int)
>   pullEachPipe [] _ vs newps nValues = (vs,newps,nValues)
>   pullEachPipe _ [] vs newps nValues = (vs,newps,nValues)
>   pullEachPipe ps (tag:tags) vs _ nValues =
>       let
>           (v, newps) = pullMatchingPipe ps tag
>           newnValues = if ( isMissing v ) then nValues else (nValues + 1)
>       in
>           pullEachPipe newps tags (vs ++ [v]) newps newnValues



>   pull :: (Monad m,MonadIO m) => Tag -> WorkflowT m Token
>   pull tag =
>       do
>           st <- get
>           let oldps = getPipes st
>           let (result,newps) = pullMatchingPipe oldps tag
>           let newSt =     WFState{
>                               getDebug = (getDebug st),
>                               getTime = (getTime st),
>                               getId = (getId st),
>                               getPipes = newps,
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = ( getOps st )
>                           }
>           put $ newSt
>           -- liftIO $ putStrLn $ " " ++ (tag) ++ ".pull >>> " ++ (take 30 (show result))
>           return result


pullAND will not return unless it can successfully pull a non-missing
value from each of its input tags.

>   pullMatchingPipes :: [Pipe] -> [Tag] -> [Token] -> [Pipe] -> ([Token],[Pipe])
>   pullMatchingPipes [] _ vs newps = (vs,newps)
>   pullMatchingPipes _ [] vs newps = (vs,newps)
>   pullMatchingPipes ps (tag:tags) vs _ =
>       let
>           (v, newps) = pullMatchingPipe ps tag
>           newvs = if (isMissing v) then
>                       vs
>                   else
>                       vs ++ [v]
>       in
>           pullMatchingPipes newps tags newvs newps


>   pullAND :: (Monad m,MonadIO m) => [Tag] -> WorkflowT m [Token]
>   pullAND tags =
>       do
>           st <- get
>           let oldps = getPipes st
>           let (pulledData, newps) = pullMatchingPipes oldps tags [] []
>           r <-    if ( ( length pulledData ) == ( length tags ) ) then
>                       do
>                           let newSt =     WFState{
>                                               getDebug = (getDebug st),
>                                               getTime = (getTime st),
>                                               getId = (getId st),
>                                               getPipes = newps,
>                                               getInPipes = (getInPipes st),
>                                               getOutPipes = (getOutPipes st),
>                                               getOps = ( getOps st )
>                                           }
>                           put $ newSt
>                           -- liftIO $ putStrLn $ (show tags) ++ ".pull >>> " ++ (take 30 (show pulledData))
>                           return pulledData
>                   else
>                       do
>                           -- liftIO $ putStrLn $ (show tags) ++ ".pull >>> []"
>                           return []
>           return r


>   getNamedPipeData :: Tag -> [Pipe] -> [Token]
>   getNamedPipeData _ [] = []
>   getNamedPipeData t ( ( Pipe{ getPipeTag=ptag, getPipeData=d} ):ps ) =
>       if ( t == ptag ) then d else (getNamedPipeData t ps)


>   getPipeContents :: (Monad m) => Tag -> WorkflowT m [Token]
>   getPipeContents t =
>       do
>           st <- get
>           return $ getWorkflowPipeData st t


>   getWorkflowPipeData :: (Monad m) => WFState m -> Tag -> [Token]
>   getWorkflowPipeData st t =
>       let
>           p = getPipes st
>       in
>           getNamedPipeData t p




========== Pipe ==


>   clearMatchingPipe :: [Pipe] -> Tag -> [Pipe] -> [Pipe]
>   clearMatchingPipe [] _ newps = newps
>   clearMatchingPipe (p:ps) tag newps =
>       let newp =
>                   if (getPipeTag p) == tag then
>                       p{ getPipeData=[] }
>                   else
>                       p
>       in do
>           clearMatchingPipe ps tag (newps ++ [newp])


>   clearPipe :: (Monad m) => Tag -> WorkflowT m ()
>   clearPipe tag =
>       do
>           st <- get
>           let oldps = getPipes st
>           let newps = clearMatchingPipe oldps tag []
>           let newSt =     WFState{
>                               getDebug = (getDebug st),
>                               getTime = (getTime st),
>                               getId = (getId st),
>                               getPipes = newps,
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = ( getOps st )
>                           }
>           put $ newSt


========== Pipe ==

=========== installActivity (legacy) and installOperator ============


This version of installActivity uses the in/out tag list to:
    - Create in/out ports on  the operator
    - Initialize these args with the corresponding in/out pipe tag

>   installActivity :: (Monad m) => (Runnable m) -> [Tag] -> [Tag] -> (WorkflowT m OperatorID)
>   installActivity act intags outtags =
>       let
>           sig = Signature (length intags, length outtags)
>       in do
>           opId <- installOperator act sig
>           bindOperatorPorts opId intags outtags
>           return $ opId



>   installOperator :: (Monad m) => (Runnable m) -> Signature -> (WorkflowT m OperatorID)
>   installOperator act (Signature (numInPorts, numOutPorts)) =
>       let
>           makePorts :: Int -> [Tag]
>           makePorts 0 = []
>           makePorts n =
>               let
>                   remainingPorts = makePorts (n - 1)
>                   unboundPort = ""
>               in
>                   unboundPort:remainingPorts
>       in do
>           st <- get
>           let as = getOps st
>           let opId = (getId st)
>           let thread = Operator{
>                                   getOperatorID           =  opId,
>                                   getOperatorInputs       = (makePorts numInPorts),
>                                   getOperatorOutputs      = (makePorts numOutPorts),
>                                   getOperatorRunnable     = act }
>           let newSt =     WFState{
>                               getDebug = (getDebug st),
>                               getTime = (getTime st),
>                               getId = (opId + 1),
>                               getPipes = (getPipes st),
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = ( as ++ [thread] )
>                           }
>           put $ newSt
>           return $ opId


>   bindOperatorPorts :: (Monad m) => OperatorID -> [Tag] -> [Tag] -> (WorkflowT m ())
>   bindOperatorPorts opId intags outtags =
>       let
>       in do
>           o <- getOperator opId
>           _ <-
>               if (length intags) /= (length (getOperatorInputs o)) then
>                   error "bindOperatorPorts mismatch"
>               else if (length outtags) /=(length (getOperatorOutputs o)) then
>                   error "bindOperatorPorts mismatch"
>               else do
>                   let newop = Operator{
>                                       getOperatorID           = opId,
>                                       getOperatorInputs       = intags,
>                                       getOperatorOutputs      = outtags,
>                                       getOperatorRunnable     = (getOperatorRunnable o) }
>                   updateOperator opId newop
>           return ()


updateOperator will replace an operator with a modified version within the
workflow state. If the opId does not exist, then there will be no effect.

>   updateOperator :: (Monad m) => OperatorID -> Operator m -> WorkflowT m ()
>   updateOperator opId newo =
>       let
>           replaceOperator _ [] _ result = result
>           replaceOperator lookforid ( o : os ) newop result =
>               replaceOperator lookforid os newop $
>                   result ++
>                   [ if lookforid == (getOperatorID o) then newo else o ]
>       in do
>           st <- get
>           let ts = getOps st
>           let newts = replaceOperator opId ts newo []
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


>   getOperator :: (Monad m) => OperatorID -> WorkflowT m (Operator m)
>   getOperator opId =
>       let
>           findOperator _ [] = Nothing
>           findOperator lookforid (o:os) =
>               if (getOperatorID o) == lookforid then (Just o) else findOperator lookforid os
>       in do
>           st <- get
>           let os = getOps st
>           let (Just o) = ( findOperator opId os )
>           return $ o





The WOOLTypes method amends the monad state by adjusting the debug component

>   debugT :: (Monad m) => String -> WorkflowT m ()
>   debugT e =
>       do
>           st <- get
>           let es = getDebug st
>           let s = getTime st
>           let exc = "[" ++ show s ++ "]  " ++ e
>           let newSt =     WFState{
>                               getDebug = (exc:es),
>                               getTime = (getTime st),
>                               getId = (getId st),
>                               getPipes = (getPipes st),
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = (getOps st)
>                           }
>           put $ newSt

>   debugTBatch :: (Monad m) => [String] -> WorkflowT m ()
>   debugTBatch msgs =
>       do
>           st <- get
>           let es = getDebug st
>           let newSt =     WFState{
>                               getDebug = ["</batch>\n"] ++ es ++ ["<batch>\n"] ++ msgs,
>                               getTime = (getTime st),
>                               getId = (getId st),
>                               getPipes = (getPipes st),
>                               getInPipes = (getInPipes st),
>                               getOutPipes = (getOutPipes st),
>                               getOps = (getOps st)
>                           }
>           put $ newSt


updateActivity will replace an operator's activity.

If the opId does not exist, then there will be no effect.

>   updateActivity :: (Monad m,MonadIO m) => OperatorID -> (Runnable m) -> WorkflowT m ()
>   updateActivity opId newact =
>       let
>           replaceOperatorActivity _ [] _ result = result
>           replaceOperatorActivity lookforid ( o : os ) newactx result =
>               let
>                   newop = if lookforid == (getOperatorID o) then
>                               Operator{
>                                   getOperatorID           =  getOperatorID o,
>                                   getOperatorInputs       = getOperatorInputs o,
>                                   getOperatorOutputs      = getOperatorOutputs o,
>                                   getOperatorRunnable         = newactx }
>                           else
>                               o
>               in
>                   replaceOperatorActivity lookforid os newact $
>                       result ++ [ newop ]
>       in do
>           st <- get
>           let ts = getOps st
>           let newts = replaceOperatorActivity opId ts newact []
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


=============== Function ===============

>   data    Function m = (Monad m) =>
>                       Function{
>                           functionEval        :: OperatorID -> [Token] -> [Token] -> WorkflowT m ([Token],[Token]),
>                           functionState       :: [Token]
>                       }

>   instance (Monad m) => Show (Function m) where
>       show a = "Function { functionState = " ++ (show (functionState a)) ++ " }"
>       -- show a = "Function { functionState = " ++ "(show (functionState a))" ++ " }"


makeFunctionMS is currently the most primitive building block for user-creation
of activities. The other makeFunctionXX methods are built upon this.
    
This is the simple, default way for a user to create an F-activity.
If you want to use the monad, you must use a more complex version like:
    makeFunctionM - Provides the monad and the opId
    makeFunctionS - Provides for activity state (NYI)
    makeFunctionMS - both (NYI)


>   makeFunctionMS :: (Monad m,MonadIO m) =>
>                       (OperatorID -> [Token] -> [Token] -> WorkflowT m ([Token],[Token])) ->
>                       [Token] -> [Tag] -> [Tag] -> (WorkflowT m OperatorID)
>   makeFunctionMS mseval state intags outtags =
>       let act = Runnable $ Function{
>                               functionEval=mseval,
>                               functionState=state
>                           }
>       in
>           installActivity act intags outtags

>   makeFunction :: (Monad m,MonadIO m) => ([Token] -> [Token]) -> [Tag] -> [Tag] -> (WorkflowT m OperatorID)
>   makeFunction feval intags outtags =
>       let
>           mseval _ state tokens = -- data is a reserved word
>               let
>                   pureVal = feval tokens
>               in
>                   return $ (state, pureVal)
>        in do
>           makeFunctionMS mseval [] intags outtags



>   makeFunctionS :: (Monad m,MonadIO m) => ([Token] -> [Token] -> ([Token],[Token])) -> [Token] -> [Tag] -> [Tag] -> (WorkflowT m OperatorID)
>   makeFunctionS seval state intags outtags =
>       let
>           mseval _ xstate tokens =
>               let
>                   (newstate,result) = seval xstate tokens
>               in do
>                   return (newstate,result)
>       in do
>           makeFunctionMS mseval state intags outtags




>   makeFunctionM :: (Monad m,MonadIO m) => (OperatorID -> [Token] -> WorkflowT m [Token]) -> [Tag] -> [Tag] -> (WorkflowT m OperatorID)
>   makeFunctionM meval intags outtags =
>       let
>           mseval opId state tokens =
>               do
>                   result <- meval opId tokens
>                   return (state,result)
>       in do
>           makeFunctionMS mseval [] intags outtags


>   newFunctionMS :: (Monad m,MonadIO m) =>
>                       (OperatorID -> [Token] -> [Token] -> WorkflowT m ([Token],[Token]) ) ->
>                       [Token] ->
>                       Signature ->
>                       (WorkflowT m OperatorID)
>   newFunctionMS mseval state sig =
>       let
>           act = Runnable $ Function{
>                               functionEval=mseval,
>                               functionState=state
>                           }
>       in
>           installOperator act sig


>   newFunction :: (Monad m,MonadIO m) => ([Token] -> [Token]) -> Signature -> (WorkflowT m OperatorID)
>   newFunction feval sig =
>       let
>           mseval _ state tokens = -- data is a reserved word
>               let
>                   pureVal = feval tokens
>               in
>                   return $ (state, pureVal)
>        in do
>           newFunctionMS mseval [] sig



>   newFunctionS :: (Monad m,MonadIO m) => ([Token] -> [Token] -> ([Token],[Token])) -> [Token] -> Signature -> (WorkflowT m OperatorID)
>   newFunctionS seval state sig =
>       let
>           mseval _ xstate tokens =
>               let
>                   (newstate,result) = seval xstate tokens
>               in do
>                   return (newstate,result)
>       in do
>           newFunctionMS mseval state sig




>   newFunctionM :: (Monad m,MonadIO m) => (OperatorID -> [Token] -> WorkflowT m [Token]) -> Signature -> (WorkflowT m OperatorID)
>   newFunctionM meval sig =
>       let
>           mseval opId state tokens =
>               do
>                   result <- meval opId tokens
>                   return (state,result)
>       in do
>           newFunctionMS mseval [] sig


>   instance (Monad m,MonadIO m) => CanRun m (Function m) where
>       run oid intags outtags act@Function{
>                       functionEval=mseval,
>                       functionState=state } =
>           let
>               writeResults :: (Monad m) => [Tag] -> [Token] -> WorkflowT m ()
>               writeResults [] _ = return ()
>               writeResults _ [] = return ()
>               writeResults (t:ts) (v:vs) = do
>                   debugT $ "writeResults: " ++ (show v)
>                   _ <-    if ( isMissing v ) then do
>                               return ()
>                           else do
>                               push t $! v
>                   writeResults ts vs
>           in
>               do
>                   _ <-    if ((length intags) == 0) then
>                               do
>                                   debugT $ "Function AutoFiring: " ++ (show act)
>                                   (newstate,results) <- mseval oid state []
>                                   writeResults outtags results
>                                   updateActivity oid $ Runnable Function {
>                                                           functionEval=(functionEval act),
>                                                           functionState=newstate }
>                                   return ()
>                           else
>                               do
>                                   args <- pullAND intags
>                                   _ <-    if ( ( length args ) > 0 ) then
>                                                do
>                                                   debugT $ "Function Firing: " ++ (show act)
>                                                   debugT $ "Function state: " ++ (show state)
>                                                   (newstate,results) <- mseval oid state args
>                                                   debugT $ "Function newstate: " ++ (show newstate)
>                                                   debugT $ "Function results: " ++ (show results)
>                                                   writeResults outtags results
>                                                   updateActivity oid $ Runnable Function {
>                                                                           functionEval=(functionEval act),
>                                                                           functionState=newstate }
>                                                   return ()
>                                            else
>                                                do
>                                                    return ()
>                                   return ()
>                   return act

